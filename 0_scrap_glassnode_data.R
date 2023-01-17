# DO NOT RUN flow and balance calculations ----
# Final output is available on repo clean_flows_and_balances_hourly.rds

library(lubridate)
library(tidyverse)
library(glue)
library(httr)
source("_config.R")

# load exchange characteristics
exchanges <- c("aggregated","binance","bitfinex","bitmex","bitstamp","bittrex","coinbase","coincheck","gemini","hitbtc","huobi","kraken","luno","okex","poloniex")

# Glassnode API scrapper
glassnode_api <- function(field = "transactions/",
                          metric = "transfers_volume_exchanges_net",
                          asset = "btc",
                          exchange ="binance",
                          format = "json",
                          api_key = Sys.getenv("glassnode_api_key"),
                          frequency = "1h"){
  url <- paste0("https://api.glassnode.com/v1/metrics/", field, metric, "?api_key=", api_key,"&a=",asset, "&e=", exchange, "&f=", format, "&i=", frequency)
  r <- GET(url)
  result <- jsonlite::fromJSON(content(r, as ="parsed")) %>% as_tibble()
  result <- result %>% 
    mutate(t = ymd_hms(as.POSIXct(t, origin ="1970-01-01")),
           exchange = exchange)
  return(result)
  }

# Inflow volume 
exchange_inflow_volume <- exchanges %>% 
  map_dfr(~glassnode_api(exchange = ., metric ="transfers_volume_to_exchanges_sum"))
# Outflow volume 
exchange_outflow_volume <- exchanges %>% 
  map_dfr(~glassnode_api(exchange = ., metric ="transfers_volume_from_exchanges_sum"))

# Daily exchange balance (as a starting point)
r <- GET(paste0("https://api.glassnode.com/v1/metrics/distribution/balance_exchanges_all?api_key",Sys.getenv("glassnode_api_key"),"&a=btc&f=json&i=24h"))
result <- jsonlite::fromJSON(content(r))
result <- bind_cols(t = result$t, result$o) %>% as_tibble()
exchange_balance_daily <- result %>% 
  pivot_longer(-t, values_to = "v", names_to = "exchange", values_drop_na = TRUE) %>% mutate(t = ymd_hms(as.POSIXct(t, origin ="1970-01-01")))

#load exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv") %>%
  select(exchange)

exchange_balance_daily <- exchange_balance_daily %>% 
  rename(timestamp = t,
         balance = v) %>% 
  inner_join(exchange_characteristics, by = "exchange") %>% 
  filter(exchange!= "aggregated",
         timestamp >= cutoff,
         timestamp <= cutoff_end) 

# flows
exchange_flows <- bind_rows(exchange_inflow_volume %>% mutate(type = "inflow"), 
                            exchange_outflow_volume %>% mutate(type = "inflow")) %>% 
  rename(timestamp = t,
         flow = v) %>% 
  inner_join(exchange_characteristics, by = "exchange") %>% 
  filter(exchange!= "aggregated",
         timestamp >= cutoff,
         timestamp <= cutoff_end) 

net_flows <- exchange_flows %>% 
  group_by(timestamp, exchange) %>% 
  arrange(type) %>% 
  filter(n() == 2) %>% 
  summarise(net_flow = first(flow) - last(flow)) %>% 
  ungroup()

# balances (hourly)
starting_balance <- exchange_balance_daily %>% 
  group_by(exchange) %>% 
  filter(row_number() == 1) %>% 
  select(-timestamp, starting_balance = balance)

flows_and_balances <- net_flows %>% 
  group_by(exchange) %>% 
  left_join(starting_balance, by = "exchange") %>% 
  mutate(balance = starting_balance + cumsum(net_flow)) %>% 
  select(-starting_balance)

write_rds(flows_and_balances, "data/glassnode/clean_flows_and_balances_hourly.rds")

# Some summary statistics ----
btc_balances <- exchange_balance_daily %>% 
  group_by(exchange) %>% 
  filter(row_number()==n()) %>% 
  pull(balance) %>% 
  sum()
prices <- read_rds("data/best_bids_n_asks.rds")
close_price <- prices %>% 
  filter(ts >= "2019-10-31") %>%
  group_by(exchange) %>% 
  filter(row_number() == n()) %>% 
  transmute(midquote = ask/2 + bid/2) %>% 
  pull(midquote) %>%
  mean()

btc_balances 
btc_balances_usd <- btc_balances * close_price / 1e9

btc_balances_growth <- exchange_balance_daily %>% 
  group_by(exchange) %>% 
  filter(row_number()==n() | row_number() == 1) %>% 
  summarise(growth = 100*(last(balance) / first(balance) - 1))

exchange_balance_daily %>% 
  group_by(exchange) %>% 
  filter(row_number()==n() | row_number() == 1) %>% 
  group_by (timestamp) %>%
  summarise(balance = sum(balance)) %>%
  summarise(growth = 100*(last(balance) / first(balance) - 1))
