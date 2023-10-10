# Exchange-pair FE regression model

library(tidyverse)
library(fixest)

source("_config.R")
regression_sample <- read_rds("data/exchange_pair_hourly_regression_sample.rds")
regression_sample <- regression_sample |>
  mutate(inventory = log(1 + balance_sell),
         latency_median_log = log(latency_median),
         latency_variance_log = log(latency_sd^2))

# label variables
dictionary <- c(
  delta = "Price Differences (in %)",
  spotvola = "Spot Volatility (in %)",
  latency_median_log = "Latency Median",
  latency_variance_log = "Latency Variance",
  boundary = "Arbitrage Bound (in %)",
  boundary_margin_sell = "Arbitrage Bound X Margin Trading",
  boundary_business_sell = "Arbitrage Bound X Business Accounts",
  spread = "Spread (in %)",
  pair = "Exchange-pair Fixed Effects",
  aa_rating_sell = "AA Rating",
  inventory = "Inventory"
)

# Price Differences and Sources of Price Risk -----------------------------
vcov <- NW(24) ~ pair + hour

pd_model1 <- feols(
  delta ~ boundary| pair+ sell_side,
  vcov = vcov,
  data = regression_sample
)

pd_model2 <- feols(
  delta ~ spotvola + latency_median_log + latency_variance_log | pair + sell_side,
  vcov = vcov,
  data = regression_sample
)

pd_model3 <- feols(
  delta ~ boundary + boundary_margin_sell | pair + sell_side,
  vcov = vcov,
  data = regression_sample
)

pd_model4 <- feols(
  delta ~ boundary + boundary_business_sell| pair + sell_side,
  vcov = vcov,
  data = regression_sample
)

pd_model5 <- feols(
  delta ~ boundary + inventory| pair,
  vcov = vcov,
  data = regression_sample
)

pd_model6 <- feols(
  delta ~  spotvola + latency_median_log + latency_variance_log + inventory | pair,
  vcov = vcov,
  data = regression_sample
)

etable(
  pd_model1, pd_model2, pd_model3, pd_model4, pd_model5, pd_model6,
  coefstat = "tstat",
  dict = dictionary
)

etable(
  pd_model1, pd_model2, pd_model3, pd_model4, pd_model5, pd_model6,
  coefstat = "tstat",
  dict = dictionary, 
  tex = TRUE, 
  style.tex = style.tex(line.top = "\\toprule", line.bottom = "\\bottomrule"), 
  fitstat=c('n'),
  fontsize = "footnotesize",
  tabular = "X",
  notes = "\\emph{Notes: }")

# Cross-Exchange Flows and Arbitrage Opportunities ------------------------
flows_model1 <- feols(
  flow_volume_usd ~ 1 | pair + sell_side | delta ~ spotvola +  latency_median_log + latency_variance_log,
  vcov = vcov,
  data = regression_sample |> 
    mutate(flow_volume_usd = replace_na(flow_volume_usd, 0))
)

flows_model2 <- feols(
  flow_volume_usd ~  1 | pair + sell_side| delta ~ boundary,
  vcov = vcov,
  data = regression_sample |> 
    mutate(flow_volume_usd = replace_na(flow_volume_usd, 0))
)

flows_model3 <- feols(
  flow_volume ~ 1 | pair + sell_side| delta ~ spotvola +  latency_median_log + latency_variance_log,
  vcov = vcov,
  data = regression_sample |>
    mutate(flow_volume = replace_na(flow_volume, 0))
)

flows_model4 <- feols(
  flow_volume ~  1 | pair+ sell_side | delta ~ boundary,
  vcov = vcov,
  data = regression_sample |>
    mutate(flow_volume = replace_na(flow_volume, 0))
)

etable(
  flows_model1, flows_model2, flows_model3, flows_model4, 
  coefstat = "tstat",
  dict = dictionary
)

# Tex output (requires some manual styling)
etable(
  flows_model1, flows_model2, flows_model3, flows_model4, 
  coefstat = "tstat",
  dict = dictionary, 
  tex = TRUE, 
  style.tex = style.tex(line.top = "\\toprule", line.bottom = "\\bottomrule"), 
  headers = list("^:_:\\emph{Dependent Variable}" = c("Exchange net inflows (in 100k USD)", "Exchange net inflows (in 100k USD)", "Exchange net inflows (in BTC)", "Exchange net inflows (in BTC)")), 
  fitstat=c('n'),
  depvar = FALSE)

# Robustness check: Rating, USDT and regional splits

robustness_model1 <- feols( # Ratings
  delta ~ boundary*aa_rating_sell + inventory| pair,
  vcov = vcov,
  data = regression_sample 
)

robustness_model2 <- feols( # Regions
  delta ~ boundary + inventory | pair,
  vcov = vcov,
  data = regression_sample |> 
    filter(region_sell == "USA" & region_buy == "USA")
)

robustness_model3 <- feols(
  delta ~ boundary + inventory | pair,
  vcov = vcov,
  data = regression_sample |> 
    filter(region_sell == "Europe" & region_buy == "Europe")
)

robustness_model4 <- feols(
  delta ~ boundary+ inventory  | pair,
  vcov = vcov,
  data = regression_sample |> 
    filter((region_sell == "Europe" |region_sell == "USA") & region_buy == region_sell)
)

robustness_model5 <- feols( # Tether
  delta ~ boundary+ inventory | pair,
  vcov = vcov,
  data = regression_sample |> filter(tether_sell == TRUE & tether_buy == tether_sell) 
)

robustness_model6 <- feols( # Tether
  delta ~ boundary + inventory | pair,
  vcov = vcov,
  data = regression_sample |> filter(tether_sell == FALSE & tether_buy == tether_sell) 
)

robustness_model7 <- feols( # Tether
  delta ~ boundary + inventory | pair,
  vcov = vcov,
  data = regression_sample |> filter(tether_buy == tether_sell) 
)

etable(
  robustness_model1, robustness_model2, robustness_model3, robustness_model4, robustness_model5, robustness_model6, robustness_model7,  
  coefstat = "tstat",
  dict = dictionary,
  group=list("Controls: Inventory and Spread" = c("Spread", "Inventory"))
)

etable(
  robustness_model1, robustness_model2, robustness_model3, robustness_model4, robustness_model5, robustness_model6, robustness_model7,  
  coefstat = "tstat",
  dict = dictionary, 
  tex = TRUE, 
  style.tex = style.tex(line.top = "\\toprule", line.bottom = "\\bottomrule"), 
  headers = list("^:_:\\emph{Dependent Variable}" = c("Price Differences (in %)", 
                                                      "Regional Price Differences (in %)", "Regional Price Differences (in %)", "Regional Price Differences (in %)",
                                                      "Price Differences (in %)", "Price Differences (in %)", "Price Differences (in %)"),
                 "Subset" = c("", "USA", "Europe", "All", "USDT", "USD", "All")), 
  fitstat=c('n'),
  depvar = FALSE,
  group=list("Controls: Inventory and Spread" = c("Spread", "Inventory")))
