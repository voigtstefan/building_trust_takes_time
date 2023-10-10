# DO NOT RUN spot volatility estimation ----
# Final output is available on repo spotvolas.rds
library(tidyverse)
library(highfrequency)

source("_config.R")

# DO NOT RUN load and prepare best bids data ----
bids <- read_rds("data/best_bids_n_asks.rds") %>%
  select(exchange, ts, price = bid)

## DO NOT RUN list of exchanges
exchanges <- bids %>%
  distinct(exchange) %>%
  .$exchange # 16 exchanges

# DO NOT RUN estimate spotvolatility ----
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

source("_config.R")

## aggregate exchange-level spotvola estimates
spotvolas_files <- list.files("data/spotvolas", full.names = TRUE)

spotvolas <- vector("list", length(spotvolas_files))
for (j in 1:length(spotvolas_files)) {
  spotvolas[[j]] <- read_rds(paste0(spotvolas_files[j]))
}
spotvolas <- bind_rows(spotvolas)

## filter out 0 spotvolas and outliers
spotvolas <- spotvolas %>%
  filter(spotvola > 0) %>%
  group_by(exchange) %>%
  mutate(lb = quantile(spotvola, 0.01),
         ub = quantile(spotvola, 0.99)) %>%
  filter(spotvola > lb & spotvola < ub) %>%
  ungroup() %>%
  select(exchange, ts, spotvola)

# Output available on repo spotvolas.rds ----
write_rds(spotvolas, "data/spotvolas.rds")

# DO NOT RUN summary stats ----

spotvolas <- read_rds("data/spotvolas.rds")

spotvolas %>% 
  summarize(mean = 100 * mean(spotvola), 
            daily_mean = sqrt(1440) * mean)
