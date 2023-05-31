# Exchange-pair FE regression model

library(tidyverse)
library(lubridate)
library(primes)

source("_config.R")

## Merge with exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv") %>%
  select(exchange, tether, margin_trading, business_accounts, region, rating_categorial) |>
  mutate(region = case_when(region == "UK" ~ "Europe", 
                            region == "Japan" ~ "Other", 
                            is.na(region) ~ "Other",
                            TRUE ~ region),
         identifier = primes::generate_n_primes(n()))

# Price differences 
arbitrage <- read_rds("data/arbitrage_data.rds")

arbitrage <- arbitrage |> 
  left_join(exchange_characteristics |> select(exchange, identifier), by = c("sell_side" = "exchange")) |>
  left_join(exchange_characteristics |> select(exchange, identifier), by = c("buy_side" = "exchange"), suffix = c(".sell",".buy")) |>
  mutate(pair = identifier.sell * identifier.buy,
         hour = floor_date(ts, "hour"),
         delta = delta * 100) |> # note: scale returns to percent
    select(ts, hour, buy_side, sell_side, pair, delta)
  
# Merge with arbitrage boundaries & sell-side spotvola
arbitrage_boundaries <- read_rds("data/arbitrage_boundaries.rds")
arbitrage_boundaries <- arbitrage_boundaries |> 
  transmute(exchange, 
            ts, 
            spotvola = spotvola, 
            boundary = boundary_crra2)

## Merge with spreads (in basis)
best_bids_n_asks <- read_rds("data/best_bids_n_asks.rds")

best_bids_n_asks <- best_bids_n_asks |> 
  mutate(spread = (ask - bid) / ask,
         btc_price = (ask + bid) / 2) |>
  select(exchange, ts, spread, btc_price)

## Merge with parametrized latencies
latencies_hourly <- read_rds("data/latencies_hourly.rds")
latencies_hourly <- latencies_hourly |> 
  select(hour = ts, latency_median = median_latency, latency_sd = sd_latency)

## Merge with hourly inflows from other exchanges 
flows_hourly <- read_rds("data/cross_exchange_flows.rds")

flows_hourly <- flows_hourly |>
  mutate(hour = floor_date(ts, "hour")) |>
  group_by(from, to, hour) |>
  summarize(volume = sum(volume * 1e-08)) |>
  ungroup() |>
  left_join(exchange_characteristics |> select(exchange, identifier), 
            by = c("from" = "exchange")) |>
  left_join(exchange_characteristics |> select(exchange, identifier), 
            by = c("to" = "exchange"), suffix = c(".sell",".buy")) |>
  mutate(pair = identifier.sell * identifier.buy) 

# NOTE: I aggregate flows to / from on a pair/hour level to obtain net flows
flows_hourly <- flows_hourly |>
  drop_na(pair) |>
  arrange(hour, pair) |>
  group_by(pair, hour) |>
  summarise(to = first(to),
            from = first(from),
            flow_volume = if_else(n() == 1, first(volume), first(volume) - last(volume))) |>
  ungroup()
  
# Merge with (lagged) hourly exchange inventories 
flows_and_balances <- read_rds("data/clean_flows_and_balances_hourly.rds") |>
  select(hour = timestamp, sell_side = exchange, balance) |>
  mutate(hour = hour - lubridate::days(1))

# Merge all files

regression_sample <- arbitrage |> 
  left_join(arbitrage_boundaries, by = c("sell_side" = "exchange", "ts")) |>
  left_join(best_bids_n_asks, by = c("sell_side" = "exchange", "ts")) |> 
  left_join(latencies_hourly, by = "hour") |>
  left_join(exchange_characteristics |> select(-identifier), 
            by = c("sell_side" = "exchange")) |>
  left_join(exchange_characteristics |> select(-identifier), 
            by = c("buy_side" = "exchange"), suffix = c(".sell",".buy")) 

regression_sample <- regression_sample |>
  left_join(flows_and_balances, by = c("sell_side", "hour")) |>
  left_join(flows_hourly, by = c("pair", "hour")) |>
  mutate(flow_volume = if_else(to == sell_side, flow_volume, -flow_volume)) |>
  select(-from, -to)
  
# Prepare regression sample ---------

trim <- function(x, cut) {
  x <- replace(
    x, x > quantile(x, 1 - cut, na.rm = T), NA
  )
  x <- replace(
    x, x < quantile(x, cut, na.rm = T), NA
  )
  return(x)
}

regression_sample_prepared <- regression_sample |> 
  # trim continuous outcome variables 
  mutate(across(
    c(delta, spotvola, spread, boundary),
    ~ trim(., 0.01)
  )) |>
  drop_na(delta, spotvola, spread, boundary, latency_median, latency_sd) |> 
  # gdax removed margin end of february 2018
  mutate(margin_trading.sell = if_else(sell_side == "gdax" & ts >= "2018-02-28", FALSE, as.logical(margin_trading.sell)),
         margin_trading.buy = if_else(buy_side == "gdax" & ts >= "2018-02-28", FALSE, as.logical(margin_trading.buy))) |> 
  mutate(
    aa_rating.buy = rating_categorial.buy == "AA",
    aa_rating.sell = rating_categorial.sell == "AA",
    boundary_margin = margin_trading.sell * boundary,
    boundary_business = business_accounts.sell * boundary,
    latency_variance = latency_sd ^ 2,
    latency_variance_std = scale(latency_variance),
    balance = replace_na(balance, 0),
    flow_volume = flow_volume * btc_price / 100000
  ) |>
  select(-btc_price)

# Summary statistics
regression_sample_prepared |> 
  select(
    delta, spotvola, median_latency, sd_latency, boundary, 
    spread, margin_trading.sell, business_accounts.sell
  ) |> 
  pivot_longer(cols = everything()) |> 
  drop_na() |>
  group_by(name) |>
  summarize(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    q05 = quantile(value, 0.05),
    q50 = quantile(value, 0.50),
    q95 = quantile(value, 0.95),
    max = max(value),
    n = n()
  )

# store regression sample

write_rds(regression_sample_prepared, "data/exchange_pair_regression_sample.rds",
          compress = "gz")

