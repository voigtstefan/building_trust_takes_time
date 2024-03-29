# Exchange-pair FE regression model

library(tidyverse)
library(primes)

source("_config.R")

## Merge with exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv") |>
  select(exchange, tether, margin_trading, business_accounts, region, rating_categorial) |>
  mutate(region = case_when(region == "UK" ~ "Europe", 
                            region == "Japan" ~ "Other", 
                            is.na(region) ~ "Other",
                            TRUE ~ region),
         identifier = primes::generate_n_primes(n()))

# Price differences 
arbitrage <- read_rds("data/arbitrage_data.rds")

arbitrage <- arbitrage |> 
  select(ts, buy_side, sell_side, delta) |>
  left_join(exchange_characteristics |> select(exchange, identifier), 
            by = c("sell_side" = "exchange")) |>
  left_join(exchange_characteristics |> select(exchange, identifier), 
            by = c("buy_side" = "exchange"), 
            suffix = c("_sell","_buy")) |>
  mutate(pair = identifier_sell * identifier_buy,
         hour = floor_date(ts, "hour"),
         delta = delta * 100) 

arbitrage <- arbitrage |>  # Hourly exchange-pair sample
  group_by(hour, pair) |>
  mutate(n = n()) |>
  arrange(desc(n)) |> 
  summarise(buy_side = first(buy_side), 
            sell_side = first(sell_side),
            delta = sum(delta)) |>
  ungroup()

# Merge with arbitrage boundaries & sell-side spotvola
arbitrage_boundaries <- read_rds("data/arbitrage_boundaries.rds")
arbitrage_boundaries <- arbitrage_boundaries |> 
  mutate(hour = floor_date(ts, "hour")) |>
  group_by(hour, sell_side = exchange) |>
  summarise(spotvola = 100 * sum(spotvola), 
            boundary = 100 * sum(boundary_crra2) / 60) |>
  ungroup()

## Merge with spreads (in basis)
best_bids_n_asks <- read_rds("data/best_bids_n_asks.rds")

best_bids_n_asks <- best_bids_n_asks |> 
  mutate(hour = floor_date(ts, "hour"),
         midquote = (ask + bid) / 2,
         spread = 100 * (ask - bid) / midquote,
         spread = if_else(spread >= 0, spread, NA)) |>
  group_by(hour, exchange) |>
  summarise(spread = mean(spread),
            btc_price = mean(midquote)) |>
  ungroup()

## Merge with parametrized latencies
latencies_hourly <- read_rds("data/latencies_hourly.rds")
latencies_hourly <- latencies_hourly |> 
  select(hour = ts, 
         latency_median = median_latency, 
         latency_sd = sd_latency)

## Merge with hourly inflows from other exchanges 
flows_hourly <- read_rds("data/cross_exchange_flows.rds")

flows_hourly <- flows_hourly |>
  mutate(hour = floor_date(ts, "hour"),
         from = if_else(from == "coinbase", "gdax", from), # Fix few inconsistencies in our exchange naming conventions
         to = if_else(to == "coinbase", "gdax", to),         
         from = str_remove(from, pattern = "\\..*"), # Fix cex.io and 
         to = str_remove(to, pattern = "\\..*")) |>
  group_by(from, to, hour) |>
  summarize(volume = sum(volume * 1e-08)) |>
  ungroup() |>
  left_join(exchange_characteristics |> 
              select(exchange, identifier), 
            by = c("from" = "exchange")) |>
  left_join(exchange_characteristics |> 
              select(exchange, identifier), 
            by = c("to" = "exchange"), 
            suffix = c("_sell","_buy")) |>
  mutate(pair = identifier_sell * identifier_buy) 

# Aggregate flows to / from on a pair/hour level to obtain net hourly pair flows
flows_hourly <- flows_hourly |>
  arrange(hour, pair, from, to) |>
  group_by(pair, hour) |>
  summarise(to = first(to),
            from = first(from),
            flow_volume = if_else(n() == 1, 
                                  first(volume), 
                                  first(volume) - last(volume))) |>
  ungroup()

# Merge with (lagged) hourly exchange inventories 
flows_and_balances <- read_rds("data/clean_flows_and_balances_hourly.rds") |>
  select(hour = timestamp, exchange, balance) |>
  mutate(hour = hour - lubridate::days(1))

# Merge all files

regression_sample <- arbitrage |> 
  left_join(arbitrage_boundaries, by = c("sell_side", "hour")) |>
  left_join(best_bids_n_asks, by = c("sell_side" = "exchange", "hour")) |> 
  left_join(latencies_hourly, by = "hour") |>
  left_join(exchange_characteristics |> select(-identifier), 
            by = c("sell_side" = "exchange")) |>
  left_join(exchange_characteristics |> select(-identifier), 
            by = c("buy_side" = "exchange"), suffix = c("_sell","_buy")) 

regression_sample <- regression_sample |>
  left_join(flows_and_balances, by = c("sell_side" = "exchange", "hour")) |>
  left_join(flows_and_balances, by = c("buy_side" = "exchange", "hour"), 
            suffix = c("_sell","_buy")) |>
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
  mutate(margin_trading_sell = if_else(sell_side == "gdax" & hour >= "2018-02-28", FALSE, as.logical(margin_trading_sell)),
         margin_trading_buy = if_else(buy_side == "gdax" & hour >= "2018-02-28", FALSE, as.logical(margin_trading_buy))) |> 
  mutate(
    aa_rating_buy = rating_categorial_buy == "AA",
    aa_rating_sell = rating_categorial_sell == "AA",
    boundary_margin_sell = margin_trading_sell * boundary,
    boundary_margin.pair = margin_trading_sell * margin_trading_buy *boundary,
    boundary_business_sell = business_accounts_sell * boundary,
    boundary_business.pair = business_accounts_sell * business_accounts_buy * boundary,
    latency_variance = latency_sd ^ 2,
    latency_variance_std = scale(latency_variance),
    latency_median_std = scale(latency_median),
    balance_sell = replace_na(balance_sell, 0),
    balance_buy = replace_na(balance_buy, 0),
    flow_volume_usd = flow_volume * btc_price / 100000
  ) |>
  select(-btc_price)

# Summary statistics
regression_sample_prepared |> 
  select(
    delta, spotvola, latency_median, latency_sd, boundary, 
    spread, margin_trading_sell, business_accounts_sell
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

write_rds(regression_sample_prepared, 
          "data/exchange_pair_hourly_regression_sample.rds",
          compress = "gz")
