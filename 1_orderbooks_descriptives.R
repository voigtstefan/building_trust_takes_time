library(tidyverse)
library(lubridate)
library(xtable)

source("_config.R")

# orderbook summary statistics ----
prices <- read_rds("data/best_bids_n_asks.rds")

## summary statistics by exchange
orderbooks <- prices %>% 
  mutate(spread = ask - bid, 
         spread_bp = 10000*(ask - bid) / ask) %>% 
  group_by(exchange) %>% 
  summarize(n = n(), 
            spread = mean(spread),
            spread_bp = mean(spread_bp))

## add exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv")

summary_statistics <- orderbooks %>% 
  left_join(exchange_characteristics %>% 
              select(exchange, region, taker_fee, btc_withdrawal_fee, 
                     no_of_confirmations, margin_trading, business_accounts, rating_categorial, tether), by = "exchange") %>%
  mutate(margin_trading = if_else(margin_trading == TRUE, "\\cmark", "\\xmark"))  %>%
  mutate(tether = if_else(tether == TRUE, "\\cmark", "\\xmark"))  %>%
    mutate(business_accounts = if_else(business_accounts == TRUE, "\\cmark", "\\xmark"))  %>%
  replace_exchange_labels()

summary_statistics <- summary_statistics |>
  select(exchange, n, spread, spread_bp, taker_fee, btc_withdrawal_fee, no_of_confirmations, margin_trading, business_accounts, region, rating_categorial, tether)

## define column labels
colnames(summary_statistics) <- c(" ", "Orderbooks", "Spread (USD)", 
                                  "Spread (bp)", "Taker Fee", "With. Fee",
                                  "Conf. ", "Margin", "Business", "Region", "Rating", "USDT")

## print to latex
print(xtable(summary_statistics, 
             digits = c(0, 0,0,2,2,2,5,0,0,0,0,0,0)), 
      include.rownames = FALSE, floating = FALSE, booktabs = TRUE,
      sanitize.colnames.function = identity,
      sanitize.text.function = identity,
      format.args = list(big.mark = ",", decimal.mark = "."),
      file = 'output/tab_orderbooks_summary.tex')
