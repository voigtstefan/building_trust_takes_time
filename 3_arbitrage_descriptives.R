library(tidyverse)
library(lubridate)

source("_config.R")

# read in arbitrage data from repository ----

exchange_characteristics <- read_csv("data/exchange_characteristics.csv") %>%
  select(exchange, 
         region, 
         rating_categorial) |>
  mutate(region = case_when(region == "UK" ~ "Europe", 
                            region == "Japan" ~ "Other",
                            is.na(region) ~ "Other",
                            TRUE ~ region)) |>
  replace_exchange_labels()
  
arbitrage <- read_rds("data/arbitrage_data.rds")
arbitrage <- arbitrage |> select(buy_side, sell_side, ts, delta_q)

arbitrage_reverse <- arbitrage %>% 
  rename(buy_side = sell_side,
         sell_side = buy_side) %>% 
  mutate(delta_q = 0)

arbitrage_hm <- bind_rows(arbitrage, arbitrage_reverse) %>% 
  group_by(buy_side, sell_side) %>% 
  summarize(delta_q = 10000 * mean(delta_q, na.rm = TRUE)) %>% 
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
  scale_y_discrete(name = 'Low-price exchange', 
                   limits = rev(levels(arbitrage_hm$buy_side))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom") + 
  labs(x = 'High-price exchange', 
       fill = expression("Mean Price Differences (in bp)"))

ggsave("output/fig_deltas_heatmap.pdf", width = fig_width, height = fig_height, units = "mm")
