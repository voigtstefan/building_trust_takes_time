library(lubridate)
library(tidyverse)
library(glue)
library(httr)

# load exchange characteristics
exchanges <- c("aggregated","binance","bitfinex","bitmex","bitstamp","bittrex","coinbase","coincheck","gemini","hitbtc","huobi","kraken","luno","okex","poloniex")

# Tiny Glassnode scrapper
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

# store everything
write_rds(exchange_balance_daily, "data/glassnode/exchange_balance_daily.rds")
write_rds(exchange_inflow_volume, "data/glassnode/exchange_inflow_volume.rds")
write_rds(exchange_outflow_volume, "data/glassnode/exchange_outflow_volume.rds")
