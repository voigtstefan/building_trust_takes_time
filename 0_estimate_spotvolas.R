# spot volatility estimation ----------------------------------------------
library(tidyverse)
library(highfrequency)

source("_config.R")

# load and prepare best bids data -----------------------------------------
bids <- read_rds("data/best_bids_n_asks.rds") %>%
  select(exchange, ts, price = bid)

## list of exchanges
exchanges <- bids %>%
  distinct(exchange) %>%
  .$exchange # 16 exchanges

# estimate spotvolatility -------------------------------------------------
## function to compute spotvola 
compute_spotvola <- function(j) {
  ## exchange-specific subsample of best bids
  data <- bids %>%
    filter(exchange == exchanges[j])
  
  ## span grid of all timestamps
  grid <- tibble(ts = as.POSIXct(seq(min(data$ts), max(data$ts), by = 60), 
                                 origin = "1970-01-01"))
  
  ## fill up grid with last available price
  prices <- grid %>%
    left_join(data, by = "ts") %>%
    arrange(ts) %>%
    fill(price, .direction = "down")
  
  ## fransform to xts object for spotvol function
  prices <- xts::as.xts(prices$price, order.by = prices$ts)
  
  # compute spotvol
  vol <- highfrequency::spotvol(prices,
                                k = 1,
                                marketopen = "00:00:00",
                                marketclose = "23:59:59",
                                on = 'minutes',
                                method = 'kernel',
                                tz = 'UTC')
  
  spotvola <- tibble(ts = zoo::index(vol$spot), 
                     spotvola = as.vector(vol$spot)) 
  
  write_rds(out, paste0("data/spotvolas/", exchanges[j], ".rds"))
  
  cat(as.character(Sys.time()), ": Done!", "\n")
}

## parallel execution
j <- as.integer(Sys.getenv("SGE_TASK_ID"))
print(exchanges[j])
compute_spotvola(j)
