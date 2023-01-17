library(tidyverse)
library(lubridate)
library(xtable)

source("_config.R")

# DO NOT RUN construct sample for arbitrage boundaries ----
# Final output is available on repo arbitrage_boundaries.rds

## load exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv") %>%
  select(exchange, no_of_confirmations) %>%
  mutate(no_of_confirmations = if_else(is.na(no_of_confirmations), 
                                       3, no_of_confirmations))

## load spot volatility estimates
spotvolas <- read_rds("data/spotvolas.rds")

## load latency parameters
latency_parameters <- read_rds("data/latency_duration_model_parameters.rds") %>%
  filter(model == "gamma" & type == "unrestricted" & 
           parameters %in% c("intercept", "alpha", "beta_1", "beta_2")) %>%
  select(date, parameters, values) %>%
  spread(parameters, values) %>%
  rename(beta_constant = intercept,
         beta_fee = beta_1,
         beta_mempool = beta_2)

latency_parameters <- latency_parameters %>%
  mutate(date = date + 1) # note: use parameters on next day

## load benchmark fees (90% quantiles of fee per byte)
fees <- read_rds("data/mempool_fees.rds")

## load number of unconfirmed transactions for each minute
mempool <- read_rds("data/mempool_size.rds") %>%
  select(ts, unconfirmed_tx = number) %>%
  filter(unconfirmed_tx > 0) %>%
  mutate(unconfirmed_tx = log(unconfirmed_tx))

## prepare data
boundary_sample <- spotvolas %>%
  left_join(exchange_characteristics, by = "exchange") %>%
  mutate(date = as.Date(ts)) %>%
  left_join(latency_parameters, by = "date") %>%
  left_join(fees, by = "ts") %>%
  left_join(mempool, by = "ts")

## drop rows without necessary information
boundary_sample <- boundary_sample %>%
  na.omit()
nrow(boundary_sample)

# DO NOT RUN compute different arbitrage boundaries ----------------------------------
## note: fees enter as fee per byte in satoshi here (different from compute_abitrage.R!)
compute_boundary <- function(fee, alpha, beta_fee, beta_mempool, beta_constant, 
                             unconfirmed_tx, spotvola, b, gamma = 2, vola = TRUE) {
  e_t <- alpha*exp(beta_constant + unconfirmed_tx*beta_mempool + beta_fee*fee)
  v_t <- alpha*exp(2*(beta_constant + unconfirmed_tx*beta_mempool + beta_fee*fee))
  c1 <- e_t + 9.65*(b-1)
  c2 <- v_t + 9.59^2*(b-1)^2 + (9.65*(b-1)+e_t)^2
  if (vola == FALSE) {
    c2 <- 0
  }
  d_t <- 1/2*spotvola*sqrt(gamma*c1+sqrt(gamma^2*c1^2+2*gamma*(gamma+1)*(gamma+2)*c2))
  return(d_t)
}
compute_boundary <- Vectorize(compute_boundary)

arbitrage_boundaries <- boundary_sample %>%
  mutate(boundary_crra2 = compute_boundary(tx_fee_per_byte_q9, alpha, beta_fee, beta_mempool, 
                                           beta_constant, unconfirmed_tx, 
                                           spotvola, no_of_confirmations, 2),
         boundary_novola = compute_boundary(tx_fee_per_byte_q9, alpha, beta_fee, beta_mempool, 
                                            beta_constant, unconfirmed_tx, 
                                            spotvola, no_of_confirmations, 2, FALSE),
         boundary_0blocks = compute_boundary(tx_fee_per_byte_q9, alpha, beta_fee, beta_mempool, 
                                              beta_constant, unconfirmed_tx, 
                                              spotvola, b=1, gamma=2))

write_rds(arbitrage_boundaries, "data/arbitrage_boundaries.rds")

# Replication starts here. Summary statistics ----
arbitrage_boundaries <- read_rds("data/arbitrage_boundaries.rds")

boundaries_table <- arbitrage_boundaries %>%
  mutate(boundary_crra = boundary_crra2 * 10000,
         boundary_0blocks = boundary_0blocks * 10000,
         boundary_novola = boundary_novola * 10000) %>%
  group_by(exchange) %>%
  summarise(Mean = mean(boundary_crra, na.rm = TRUE),
            SD = sd(boundary_crra, na.rm = TRUE),
            '5\\%' = quantile(boundary_crra, 0.05, na.rm = TRUE), 
            '25\\%' = quantile(boundary_crra, 0.25, na.rm = TRUE), 
            Median = median(boundary_crra, na.rm = TRUE), 
            '75\\%' = quantile(boundary_crra, 0.75, na.rm = TRUE), 
            '95\\%' = quantile(boundary_crra, 0.95, na.rm = TRUE),
            'Security' = 100*(1 - median(boundary_0blocks, na.rm = TRUE) / Median),
            'Uncertainty' = 100*(1 - median(boundary_novola, na.rm = TRUE) / Median)) %>%
  replace_exchange_labels() %>%
  rename(' ' = 'exchange')

#colnames(boundaries_table) <- paste0("\\multicolumn{1}{c}{", 
#                                     colnames(boundaries_table), "}")

print(xtable(boundaries_table), 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      floating = FALSE,
      sanitize.text.function = function(x) {x},
      file = 'output/tab_arbitrage_boundaries_summary.tex')

## summary statistics
tibble(mean = round(mean(boundaries_table$Mean), 2),
                 uncertainty = round(mean(boundaries_table$Uncertainty), 2),
                 security = round(mean(boundaries_table$Security), 2),
                 blocks_0 = round(mean(arbitrage_boundaries$boundary_0blocks, na.rm = TRUE) * 10000, 2))
