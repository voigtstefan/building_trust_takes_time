# Exchange-pair FE regression model

library(tidyverse)
library(fixest)

source("_config.R")
regression_sample <- read_rds("data/exchange_pair_hourly_regression_sample.rds")

# label variables
dictionary <- c(
  delta = "Price Differences (in %)",
  spotvola = "Spot Volatility (in %)",
  latency_median = "Latency Median (in Min)",
  latency_median_std = "Latency Median (Standardized)",
  latency_variance_std = "Latency Variance (Standardized)",
  boundary = "Arbitrage Bound (in %)",
  boundary_margin = "Arbitrage Bound X Margin Trading",
  boundary_business = "Arbitrage Bound X Business Accounts",
  spread = "Spread (in %)",
  margin_trading = "Margin Trading",
  business_accounts = "Business Accounts",
  pair = "Exchange-pair Fixed Effects",
  aa_rating = "AA Rating"
)

# Price Differences and Sources of Price Risk -----------------------------
vcov <- "hetero"

pd_model1 <- feols(
  delta ~ boundary +spread| pair,
  vcov = vcov,
  data = regression_sample 
)

pd_model2 <- feols(
  delta ~ spotvola + log(latency_median) + log(latency_sd)+spread| pair,
  vcov = vcov,
  data = regression_sample 
)

pd_model3 <- feols(
  delta ~ boundary + boundary_margin.pair +spread| pair,
  vcov = vcov,
  data = regression_sample 
)

pd_model4 <- feols(
  delta ~ boundary + boundary_business.sell +spread| pair,
  vcov = vcov,
  data = regression_sample 
)

pd_model5 <- feols(
  delta ~ boundary + log(1+balance.sell) +spread| pair,
  vcov = vcov,
  data = regression_sample
)

pd_model6 <- feols(
  delta ~  spotvola + log(latency_median) + log(latency_sd) + log(1+balance.sell) +spread| pair,
  vcov = vcov,
  data = regression_sample 
)

etable(
  pd_model1, pd_model2, pd_model3, pd_model4, pd_model5, pd_model6,
  coefstat = "tstat",
  dict = dictionary
)

etable(
  pd_model1, pd_model2, pd_model3, pd_model4,
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
  flow_volume_usd ~ spread | pair| delta ~ spotvola +  log(latency_median) + log(latency_sd),
  vcov = vcov,
  data = regression_sample
)

flows_model2 <- feols(
  flow_volume_usd ~  spread | pair | delta ~ boundary,
  vcov = vcov,
  data = regression_sample
)

flows_model3 <- feols(
  log(flow_volume_usd) ~ spread | pair | delta ~ spotvola +  log(latency_median) + log(latency_sd),
  vcov = vcov,
  data = regression_sample
)

flows_model4 <- feols(
  log(flow_volume_usd) ~  spread | pair| delta ~ boundary,
  vcov = vcov,
  data = regression_sample
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
  headers = list("^:_:\\emph{Dependent Variable}" = c("Exchange Inflows (in 100k USD)", "Exchange Inflows (in 100k USD)", "Log(Exchange Inflows)", "Log(Exchange Inflows)")), 
  fitstat=c('n'),
  depvar = FALSE)

# Robustness check: Rating, USDT and regional splits

robustness_model1 <- feols( # Ratings
  delta ~ boundary*aa_rating.sell + log(1+balance.sell) + spread | pair,
  vcov = vcov,
  data = regression_sample 
)

robustness_model2 <- feols( # Regions
  delta ~ boundary + log(1+balance.sell) + spread | pair,
  vcov = vcov,
  data = regression_sample |> 
    filter(region.sell == "USA" & region.buy == "USA")
)

robustness_model3 <- feols(
  delta ~ boundary + log(1+balance.sell) + spread| pair,
  vcov = vcov,
  data = regression_sample |> 
    filter(region.sell == "Europe" & region.buy == "Europe")
)

robustness_model4 <- feols(
  delta ~ boundary+ log(1+balance.sell) + spread | pair,
  vcov = vcov,
  data = regression_sample |> 
    filter((region.sell == "Europe" |region.sell == "USA") & region.buy == region.sell)
)

robustness_model5 <- feols( # Tether
  delta ~ boundary+ log(1+balance.sell) + spread | pair,
  vcov = vcov,
  data = regression_sample |> filter(tether.sell == TRUE & tether.buy == tether.sell) 
)

robustness_model6 <- feols( # Tether
  delta ~ boundary + log(1+balance.sell) + spread| pair,
  vcov = vcov,
  data = regression_sample |> filter(tether.sell == FALSE & tether.buy == tether.sell) 
)

robustness_model7 <- feols( # Tether
  delta ~ boundary + log(1+balance.sell) + spread| pair,
  vcov = vcov,
  data = regression_sample |> filter(tether.buy == tether.sell) 
)

etable(
  robustness_model1, robustness_model2, robustness_model3, robustness_model4, robustness_model5, robustness_model6, robustness_model7,  
  coefstat = "tstat",
  dict = dictionary,
  group=list("Controls: Inventory and Spread" = c("Spread", "Inventory"))
)

etable(
  pd_model13, pd_model7, pd_model8, pd_model9, pd_model10, pd_model11, pd_model12,  
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
