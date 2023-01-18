library(tidyverse)
library(lubridate)

source("_config.R")

# load data ---------------------------------------------------------------

## load exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv") %>%
  select(exchange, 
         no_of_confirmations, 
         tether, 
         margin_trading, 
         business_accounts, 
         region, 
         rating_categorial) |>
  mutate(region = case_when(region == "UK" ~ "Europe", 
                            region == "Japan" ~ "Other", 
                            is.na(region) ~ "Other",
                            TRUE ~ region))

#hourly price differences 
arbitrage <- read_rds("data/arbitrage_data.rds") |>
  mutate(ts = floor_date(ts, "hour"))

arbitrage_hourly <- arbitrage %>%
  group_by(sell_side, ts) %>%
  # note: scale returns to percent
  summarize(delta = sum(delta) * 100) %>%
  ungroup() %>%
  # note: compute average hourly arbitrage opportunities
  mutate(delta = delta / 60)

arbitrage_region_hourly <- arbitrage |>
  select(buy_side:delta) |>
  left_join(exchange_characteristics |> rename(sell_region = region), by = c("sell_side" = "exchange")) |>
  left_join(exchange_characteristics |> rename(buy_region = region), by = c("buy_side" = "exchange")) |>
  filter(sell_region == buy_region) |>
  group_by(sell_side, ts) %>%
  # note: scale returns to percent
  summarize(delta = sum(delta) * 100) %>%
  ungroup() %>%
  # note: compute average hourly arbitrage opportunities
  mutate(delta = delta / 60)

arbitrage_hourly <- arbitrage_hourly |> 
  left_join(arbitrage_region_hourly |> 
              rename(delta_regional = delta), by = c("sell_side", "ts"))

rm(arbitrage)

## hourly arbitrage boundaries
arbitrage_boundaries <- read_rds("data/arbitrage_boundaries.rds")

arbitrage_boundaries_hourly <- arbitrage_boundaries %>%
  mutate(ts = floor_date(ts, "hour")) %>%
  group_by(sell_side = exchange, ts) %>%
  summarize(boundary = sum(boundary_crra2, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  # note: compute average hourly arbitrage opportunities
  mutate(boundary = boundary / 60)

rm(arbitrage_boundaries)

## compute hourly spreads and hourly prices
best_bids_n_asks <- read_rds("data/best_bids_n_asks.rds")

spreads <- best_bids_n_asks %>%
  mutate(spread = (ask - bid) / ask,
         mid = (ask + bid) / 2) %>%
  select(sell_side = exchange, ts, spread, mid)

spreads_hourly <- spreads %>%
  mutate(ts = floor_date(ts, "hour")) %>%
  group_by(sell_side, ts) %>%
  summarize(spread = mean(spread) * 100,
            btc_price = mean(mid)) %>%
  ungroup()

## compute hourly volatilities
spotvolas <- read_rds("data/spotvolas.rds")

spotvolas_hourly <- spotvolas %>%
  mutate(ts = floor_date(ts, "hour")) %>%
  group_by(sell_side = exchange, ts) %>%
  summarize(spotvola = mean(spotvola, na.rm = TRUE) * 100) %>%
  ungroup()

## load hourly observed latencies
latencies_hourly <- read_rds("data/latencies_hourly.rds")

## compute hourly inflows from other exchanges 
flows <- read_rds("data/cross_exchange_flows.rds") %>%
  filter(ts >= "2018-01-01" & ts <= "2019-10-31")

inflows_hourly <- flows %>%
  mutate(ts = floor_date(ts, "hour")) %>%
  group_by(from, to, ts) %>%
  summarize(volume = sum(volume * 1e-08)) %>%
  ungroup() %>%
  group_by(sell_side = to, ts) %>%
  summarize(inflows = sum(volume)) %>%
  ungroup()

# Load (lagged) hourly exchange inventories 
flows_and_balances <- read_rds("data/clean_flows_and_balances_hourly.rds") |>
  select(ts = timestamp, sell_side = exchange, balance) |>
  mutate(ts = ts - lubridate::days(1))

# construct regression sample ----
## construct full grid to ensure that exchanges have 0s if there is no arbitrage
full_grid <- crossing(distinct(arbitrage_hourly, sell_side), 
                      distinct(spotvolas_hourly, ts))

regression_sample <- full_grid %>%
  left_join(arbitrage_hourly, by = c("sell_side", "ts")) |>
  mutate(delta = replace_na(delta, 0),
         delta_regional = replace_na(delta_regional, 0))

## put everything together
regression_sample <- regression_sample %>%
  left_join(arbitrage_boundaries_hourly, by = c("sell_side", "ts")) %>%
  left_join(spreads_hourly, by = c("sell_side", "ts")) %>%
  left_join(spotvolas_hourly, by = c("sell_side", "ts")) %>%
  left_join(latencies_hourly, by = "ts") %>%
  left_join(inflows_hourly, by = c("sell_side", "ts")) %>%
  left_join(flows_and_balances, by = c("sell_side", "ts")) |>
  left_join(exchange_characteristics, by = c("sell_side" = "exchange"))

regression_sample <- regression_sample |>
  mutate(inflows = replace_na(inflows, 0))

write_csv(regression_sample, "data/regression_sample.csv")

