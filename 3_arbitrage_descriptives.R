library(tidyverse)
library(lubridate)
library(scales)
library(xtable)

source("_config.R")

# The following code lines are just legacy code to generate the file arbitrage_data.rds
# load data (prepare for repository --------------------------------------------
# arbitrage_files <- list.files("data/arbitrage", full.names = TRUE)
# 
# arbitrage <- vector("list", length(arbitrage_files))
# for (j in 1:length(arbitrage_files)) {
#   arbitrage[[j]] <- read_rds(paste0(arbitrage_files[j]))
# }
# arbitrage <- bind_rows(arbitrage)
# 
# ## keep only observations with valid spotvola estimates
# arbitrage <- arbitrage %>% 
#   filter(is.finite(spotvola)) 
# 
# arbitrage <- arbitrage %>% 
#   mutate(date = as.Date(floor_date(ts, "day")))
# 
# arbitrage |> write_rds("data/arbitrage_data.rds")

# read in arbitrage data from repository ----

arbitrage <- read_rds("data/arbitrage_data.rds")

# heat map of price differences -------------------------------------------
## add 0 rows (reverse direction) to make it easier to compute mean
arbitrage_reverse <- arbitrage %>% 
  rename(buy_side = sell_side,
         sell_side = buy_side) %>% 
  mutate(delta_q = 0,
         delta_q_excess = 0)

arbitrage_hm <- bind_rows(arbitrage, arbitrage_reverse) %>% 
  group_by(buy_side, sell_side) %>% 
  summarize(delta_q = 10000*mean(delta_q, na.rm = TRUE)) %>% 
  ungroup() %>%
  rename(exchange = buy_side) %>%
  replace_exchange_labels() %>%
  rename(buy_side = exchange,
         exchange = sell_side) %>%
  replace_exchange_labels() %>% 
  rename(sell_side = exchange) %>% 
  mutate(buy_side = as.factor(buy_side),
         sell_side = as.factor(sell_side)) 

p1 <- ggplot(arbitrage_hm, aes(sell_side, buy_side)) + 
  geom_tile(aes(fill = delta_q), colour = "white") + 
  scale_fill_gradient(low = "white", high = "#377EB8")  +
  scale_y_discrete(name = 'Buy-Side', 
                   limits = rev(levels(arbitrage_hm$buy_side))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom") + 
  labs(x = 'Sell-Side', 
       fill = expression("Mean Price Differences (in bp)"))

ggsave('output/fig_deltas_heatmap.pdf',
       plot = p1,
       width = fig_width,
       height = fig_height,
       units = 'mm')
