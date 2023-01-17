library(tidyverse)
library(lubridate)
library(xtable)

source("_config.R") 

# DO NOT RUN Bitcoin mempool summary statistics ----
# transaction data is about 63 GB, thus not available on Github. The files in data/transactions can be provided upon request. 
# Final output is available in latencies_hourly.rds

## load daily transaction files
transactions_files <- list.files("data/transactions/", full.names = TRUE)

transactions <- vector("list", length(transactions_files))
for (j in 1:length(transactions_files)) {
  transactions[[j]] <- read_rds(paste0(transactions_files[j]))
  cat(j, "\n")
}
transactions <- bind_rows(transactions)

## compute summary table
tx_summary <- transactions %>%
  select(-c(tx_hash, ts, block_hash, date, tx_time, block_time, tx_fee)) %>%
  mutate(unconfirmed_tx = unconfirmed_tx / 1000) %>%
  na.omit() %>%
  rename(`Latency (in Min)` = tx_latency,
         `Fee per Byte (in Satoshi)` = tx_fee_per_byte,
         `Fee per Transaction (in USD)` = tx_fee_usd,
         `Transaction Size (in Bytes)` = tx_size,
         `Mempool Size (in K)` = unconfirmed_tx) %>% 
  gather(variable, value) %>% 
  group_by(variable) %>% 
  summarise(Mean = mean(value),
            SD = sd(value),
            `5 %` = quantile(value, 0.05), 
            `25 %` = quantile(value, 0.25), 
            Median = median(value), 
            `75 %` = quantile(value, 0.75), 
            `95 %` = quantile(value, 0.95))

## format and save summary table table 
colnames(tx_summary)[1] <- " "

print(xtable(tx_summary), 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      NA.string = "-",
      floating = FALSE,
      file = 'output/tab_mempool_summary.tex')

# DO NOT RUN summary statistics for regression ----
latencies_hourly <- transactions %>% 
  mutate(ts = floor_date(ts, "hour"))  %>%
  group_by(ts) %>%
  summarise(mean_latency = mean(tx_latency, na.rm = TRUE),
            median_latency = median(tx_latency, na.rm = TRUE),
            sd_latency = sd(tx_latency, na.rm = TRUE)) %>%
  ungroup()

write_rds(latencies_hourly, "data/latencies_hourly.rds")