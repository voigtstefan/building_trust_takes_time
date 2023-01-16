library(tidyverse)
source("_config.R")

# load data ---------------------------------------------------------------
## load list of orderbooks 
orderbooks <- list.files("data/orderbooks/", full.names = TRUE)

## load exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv")  %>%
  select(exchange, taker_fee, btc_withdrawal_fee, no_of_confirmations) %>%
  mutate(no_of_confirmations = if_else(is.na(no_of_confirmations), 
                                       3, no_of_confirmations),
         btc_withdrawal_fee = if_else(is.na(btc_withdrawal_fee), 
                                      0, btc_withdrawal_fee)) 

## load spot volatilities
spotvolas <- read_rds("data/spotvolas.rds")

## load latency parameters
latency_parameters <- list.files("data/latency", full.names = TRUE) %>%
  map(read_rds) %>%
  reduce(bind_rows) %>%
  filter(model == "gamma" & type == "unrestricted" & 
           parameters %in% c("intercept", "alpha", "beta_1", "beta_2")) %>%
  select(date, parameters, values) %>%
  spread(parameters, values) %>%
  rename(beta_constant = intercept,
         beta_fee = beta_1,
         beta_mempool = beta_2)

latency_parameters <- latency_parameters %>%
  mutate(date = date + 1) # note: use parameters on next day

## load number of unconfirmed transactions for each minute
mempool <- read_rds("data/mempool_size.rds") %>%
  select(ts, unconfirmed_tx = number) %>%
  mutate(unconfirmed_tx = log(unconfirmed_tx))

## construct grid of latency-related parameters
latency_parameters <- latency_parameters %>%
  left_join(mempool %>%
              mutate(date = as.Date(ts)), by = "date") %>%
  select(-date)

################################################################################
# define functions for optimization ---------------------------------------
## find the average price for quantity q 
find_price <- function(q, prices, sizes){
  f_i <- suppressWarnings(max(0, min(length(sizes) -1 , max(which(cumsum(sizes) < q)))))
  (sum(sizes[1:f_i]*prices[1:f_i]) + prices[f_i+1]*(q - sum(sizes[1:f_i]))) / q
}

## optimization (optimal q given f, then gridding for f)
objective <- function(q, f, asks, bids, buy_fee, sell_fee, withdrawal_fee) {
  ask <- find_price(q+f, asks$price, asks$size)*(1+buy_fee)
  bid <- find_price(q-withdrawal_fee, bids$price, bids$size)*(1-sell_fee)
  ret <- log(bid) - log(ask)
  obj <- (q-withdrawal_fee)*bid - (q+f)*ask
  return(c(ret, obj))
}

objective <- Vectorize(objective, vectorize.args = "q")

optimal_q_for_f <- function(f, asks, bids, buy_fee, sell_fee, withdrawal_fee, max_q){
  res <- suppressWarnings(optimize(function(x) {-objective(q=x, f, asks, bids, buy_fee, sell_fee, withdrawal_fee)[2]},
                                   interval = c(withdrawal_fee, max_q)))
  return(res$minimum)
}

boundary <- function(f, latency, spotvola, b, gamma = 2) {
  e_t = latency$alpha*exp(latency$beta_constant + 
                            latency$unconfirmed_tx*latency$beta_mempool + latency$beta_fee*f*1e8/247)
  v_t = latency$alpha*exp(2*(latency$beta_constant + 
                               latency$unconfirmed_tx*latency$beta_mempool + latency$beta_fee*f*1e8/247))
  c1 <- e_t + 9.65*(b-1)
  c2 <- v_t + 9.59^2*(b-1)^2 + (9.65*(b-1)+e_t)^2
  d_t <- 1/2*spotvola*sqrt(gamma*c1+sqrt(gamma^2*c1^2+2*gamma*(gamma+1)*(gamma+2)*c2))
  return(d_t)
}

optimal_q_f <- function(data_sub, buy_side, sell_side, latency, spotvola, b, max_q) {
  asks <- data_sub %>%
    filter(exchange == buy_side & side == "ask") %>%
    filter(!is.na(price))
  
  bids <- data_sub %>%
    filter(exchange == sell_side & side == "bid")  %>%
    filter(!is.na(price))
  
  buy_fee <- exchange_characteristics[exchange_characteristics$exchange == buy_side, ]$taker_fee / 100
  sell_fee <- exchange_characteristics[exchange_characteristics$exchange == sell_side, ]$taker_fee / 100
  withdrawal_fee <- exchange_characteristics[exchange_characteristics$exchange == buy_side, ]$btc_withdrawal_fee
  
  ## try without any fees
  f_init <- 1.53 / 1e8 # note: convert from satoshi to bitcoin
  if (max_q > withdrawal_fee) {
    q_init <- optimal_q_for_f(f_init, asks, bids, buy_fee, sell_fee, withdrawal_fee, max_q)
    obj_init <- objective(q_init, f_init, asks, bids, buy_fee, sell_fee, withdrawal_fee)
    
    if (obj_init[2] > 0) {
      ## if returns exceed risk, then we are good
      constraint <- obj_init[1] - boundary(f_init, latency, spotvola, b)
      if (constraint >= 0) {
        out <- c(obj_init, q_init, f_init)
        
        ## if returns for max q do not compensate for risk, we look for f 
      } else {
        f_obj <- function(x) {
          q <- optimal_q_for_f(x, asks, bids, buy_fee, sell_fee, withdrawal_fee, max_q)
          objective(q, x, asks, bids, buy_fee, sell_fee, withdrawal_fee)[1] - boundary(x, latency, spotvola, b)
        }
        f_obj <- Vectorize(f_obj)
        
        res <- rootSolve::uniroot.all(f_obj, c(1.53 / 1e8, 282 / 1e8)) # values taken from summary statistics 
        
        f_out <- suppressWarnings(min(res))
        q_out <- optimal_q_for_f(f_out, asks, bids, buy_fee, sell_fee, withdrawal_fee, max_q) 
        obj_out <- objective(q_out, f_out, asks, bids, buy_fee, sell_fee, withdrawal_fee)
        
        out <- c(obj_out, q_out, f_out)
      }
    } else {
      out <- c(0, 0, 0, Inf)
    }
  } else {
    out <- c(0, 0, 0, Inf)
  }
  return(out)
}

compute_arbitrage <- function(j) {
  ## load orderbook
  data <- read_rds(orderbooks[j])
  
  ## extract vector of timestamps
  timestamps <- data %>%
    distinct(ts)
  
  arbitrage <- vector("list", nrow(timestamps))
  for (t in 1:nrow(timestamps)) {
    ## extract orderbook for specific timestamp
    data_sub <- data %>%
      filter(ts == timestamps$ts[t])
    
    ## extract relevant latency parameters
    latency_parameters_sub <- latency_parameters %>%
      filter(ts == timestamps$ts[t])
    
    # compute deltas (instantaneous returns) to get trading directions
    best <- data_sub %>%
      filter(level == 1) %>%
      select(exchange, ts, side, price) %>%
      spread(side, price)
    
    deltas <- best %>%
      left_join(best, by = "ts") %>%
      filter(exchange.x != exchange.y) %>%
      mutate(delta = pmax(0, log(bid.x) - log(ask.y), log(bid.y) - log(ask.x)),
             buy_side = case_when(delta > 0 & 
                                    log(bid.x) - log(ask.y) < log(bid.y) - log(ask.x) ~ exchange.x,
                                  delta > 0 & 
                                    log(bid.x) - log(ask.y) > log(bid.y) - log(ask.x) ~ exchange.y,
                                  TRUE                                                ~ as.character(NA)),
             sell_side = case_when(delta > 0 & 
                                     log(bid.x) - log(ask.y) > log(bid.y) - log(ask.x) ~ exchange.x,
                                   delta > 0 & 
                                     log(bid.x) - log(ask.y) < log(bid.y) - log(ask.x) ~ exchange.y,
                                   TRUE                                                ~ as.character(NA))) %>%
      filter(delta > 0) %>%
      distinct(buy_side, sell_side, ts, delta)
    
    # add maxium trading sizes based on available orderbook depth
    max_depth <- data_sub %>%
      group_by(exchange, side) %>%
      mutate(depth = cumsum(size)) %>%
      summarize(max = max(depth)) %>%
      spread(side, max) %>%
      ungroup() %>%
      rename(max_ask = ask, max_bid = bid)
    
    deltas <- deltas %>%
      left_join(max_depth, by = c("buy_side" = "exchange")) %>%
      left_join(max_depth, by = c("sell_side" = "exchange")) %>%
      mutate(max_q = pmin(max_ask.x, max_bid.y)) %>%
      select(-c(max_ask.x:max_bid.y))
    
    # add sell-side spotvolatility
    spotvolas_sub <- spotvolas %>% 
      filter(ts == timestamps$ts[t]) %>%
      select(-ts)
    
    deltas <- deltas %>%
      left_join(spotvolas_sub, by = c("sell_side" = "exchange")) 
    
    # add sell-side confirmations
    deltas <- deltas %>%
      left_join(exchange_characteristics %>%
                  select(exchange, no_of_confirmations), by = c("sell_side" = "exchange"))
    
    # initialize output columns
    deltas <- deltas %>%
      mutate(delta_q = as.numeric(NA),
             dollar_return = as.numeric(NA),
             q = as.numeric(NA),
             f = as.numeric(NA),
             boundary = as.numeric(NA))
    
    if (nrow(latency_parameters_sub) > 0) {
      # loop over exchange pairs
      if (nrow(deltas) > 0) {
        for (d in 1:nrow(deltas)) {
          if(deltas$delta[d] > 0){
            if (!is.na(deltas$spotvola[d])) {
              val <- optimal_q_f(data_sub, deltas$buy_side[d], deltas$sell_side[d], 
                                 latency_parameters_sub, deltas$spotvola[d], deltas$no_of_confirmations[d], deltas$max_q[d])
              
              if (is.infinite(val[4])) {
                deltas$delta_q[d] <- 0
                deltas$dollar_return[d] <- 0
                deltas$q[d] <- 0
                deltas$f[d] <- 0
                deltas$boundary[d] <- boundary(1.53 / 100000000, latency_parameters_sub, 
                                               deltas$spotvola[d], deltas$no_of_confirmations[d])
              } else {
                deltas$delta_q[d] <- val[1]
                deltas$dollar_return[d] <- val[2]
                deltas$q[d] <- val[3]
                deltas$f[d] <- val[4]
                deltas$boundary[d] <- boundary(val[4], latency_parameters_sub, deltas$spotvola[d], 
                                               deltas$no_of_confirmations[d])
              }
            } else {
              deltas$delta_q[d] <- as.numeric(NA)
              deltas$dollar_return[d] <- as.numeric(NA)
              deltas$q[d] <- as.numeric(NA)
              deltas$f[d] <- as.numeric(NA)
              deltas$boundary[d] <- as.numeric(NA)
            }
            cat(d, "\t", t, "\t return = ", round(deltas$delta_q[d], 4), "\t objective = ", round(deltas$dollar_return[d], 2),
                " \t q = ", round(deltas$q[d], 2), " \t f = ", round(deltas$f[d], 4), "\n", "\t d = ", round(deltas$boundary[d], 4), "\n ")
          }
        }
      }
    }
    arbitrage[[t]] <- deltas
  }
  
  output <- bind_rows(arbitrage)
  
  write_rds(output, gsub("orderbooks", "arbitrage", orderbooks[j]))
}

## parallel execution
j <- as.integer(Sys.getenv("SGE_TASK_ID"))
print(orderbooks[j])
compute_arbitrage(j)
warnings()
