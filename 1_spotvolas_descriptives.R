library(tidyverse)
library(lubridate)
library(xtable)
library(scales)

source("_config.R")

# # descriptives of spotvolatility estimates --------------------------------
# ## aggregate exchange-level spotvola estimates
# spotvolas_files <- list.files("data/spotvolas", full.names = TRUE)
# 
# spotvolas <- vector("list", length(spotvolas_files))
# for (j in 1:length(spotvolas_files)) {
#   spotvolas[[j]] <- read_rds(paste0(spotvolas_files[j]))
# }
# spotvolas <- bind_rows(spotvolas)
# 
# ## filter out 0 spotvolas and outliers
# spotvolas <- spotvolas %>%
#   filter(spotvola > 0) %>%
#   group_by(exchange) %>%
#   mutate(lb = quantile(spotvola, 0.01),
#          ub = quantile(spotvola, 0.99)) %>%
#   filter(spotvola > lb & spotvola < ub) %>%
#   ungroup() %>%
#   select(exchange, ts, spotvola)
# 
# # Output available on repo ----
# write_rds(spotvolas, "data/spotvolas.rds")

# summary stats ----

spotvolas <- read_rds("data/spotvolas.rds")

spotvolas %>% 
            summarize(mean = 100 * mean(spotvola), 
                      daily_mean = sqrt(1440) * mean)
