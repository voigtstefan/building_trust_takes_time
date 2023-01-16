library(tidyverse)

# evaluation of glassnode data
source("_config.R")

#load exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv") %>%
  select(exchange)

# load glassnode data 
# balances 
exchange_balance_daily <- read_rds("data/glassnode/exchange_balance_daily.rds")

exchange_balance_daily <- exchange_balance_daily %>% 
  rename(timestamp = t,
         balance = v) %>% 
  inner_join(exchange_characteristics, by = "exchange") %>% 
  filter(exchange!= "aggregated",
         timestamp >= cutoff,
         timestamp <= cutoff_end) 

# flows
exchange_inflow_volume <- read_rds("data/glassnode/exchange_inflow_volume.rds")
exchange_outflow_volume <- read_rds("data/glassnode/exchange_outflow_volume.rds")

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
