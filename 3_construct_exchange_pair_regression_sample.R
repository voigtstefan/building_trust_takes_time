# Exchange-pair FE regression model

library(tidyverse)
library(lubridate)

source("_config.R")

# load data ---------------------------------------------------------------


# Price differences 
arbitrage <- read_rds("data/arbitrage_data.rds")
arbitrage <- arbitrage |> 
  head(1000000) |>
  select(ts, buy_side, sell_side, delta) |> 
  mutate(pair = map2(buy_side, sell_side,c), 
         pair = map(pair, str_sort), 
         pair = map_chr(pair, paste,collapse = "_"),
         hour = floor_date(ts, "hour"))

# TO DISCUSS: WHO IS SELL-SIDE EXCHANGE IF DELTA = 0? I removed the full-grid after all ..

# Merge with arbitrage boundaries & sell-side spotvola
arbitrage_boundaries <- read_rds("data/arbitrage_boundaries.rds")
arbitrage_boundaries <- arbitrage_boundaries |> 
  select(exchange, ts, spotvola, boundary = boundary_crra2)

## Merge with spreads (in basis)
best_bids_n_asks <- read_rds("data/best_bids_n_asks.rds")

best_bids_n_asks <- best_bids_n_asks |> 
  mutate(spread = 10000 * (ask - bid) / (ask / 2 + bid / 2)) |>
  select(exchange, ts, spread)

## Merge with parametrized latencies
latencies_hourly <- read_rds("data/latencies_hourly.rds")
latencies_hourly <- latencies_hourly |> 
  select(hour = ts, median_latency, sd_latency)

## Merge with exchange characteristics
exchange_characteristics <- read_csv("data/exchange_characteristics.csv") %>%
  select(exchange, tether, margin_trading, business_accounts, region, rating_categorial) |>
  mutate(region = case_when(region == "UK" ~ "Europe", 
                            region == "Japan" ~ "Other", 
                            is.na(region) ~ "Other",
                            TRUE ~ region))

regression_sample <- arbitrage |> 
  left_join(arbitrage_boundaries, by = c("sell_side" = "exchange", "ts")) |>
  left_join(best_bids_n_asks, by = c("sell_side" = "exchange", "ts")) |> 
  left_join(latencies_hourly, by = "hour") |>
  left_join(exchange_characteristics, by = c("sell_side" = "exchange"))

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
    c(delta, spotvola, spread,
      boundary),
    ~ trim(., 0.01)
  )) |>
  drop_na(delta, spotvola, spread, boundary, median_latency, sd_latency) |> 
  # gdax removed margin end of february 2018
  mutate(margin_trading = if_else(sell_side == "gdax" & ts >= "2018-02-28", FALSE, as.logical(margin_trading))) |> 
  mutate(
    aa_rating = rating_categorial == "AA",
    boundary_margin = margin_trading * boundary,
    boundary_business = business_accounts * boundary,
    latency_variance = sd_latency ^ 2,
    latency_variance_std = scale(latency_variance)
  )

# store regression sample

write_csv(regression_sample, "data/exchange_pair_regression_sample.csv.gz")

