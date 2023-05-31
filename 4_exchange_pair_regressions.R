# Exchange-pair FE regression model

library(tidyverse)
library(fixest)

source("_config.R")
regression_sample <- read_rds("data/exchange_pair_regression_sample.rds")

# label variables
dictionary <- c(
  delta = "Price Differences (in %)",
  spotvola = "Spot Volatility (in %)",
  median_latency = "Latency Median (in Min)",
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
  delta ~ boundary + spread | pair,
  vcov = ~pair,
  data = regression_sample 
)

pd_model2 <- feols(
  delta ~ spotvola + latency_median + latency_variance_std + spread | pair,
  vcov = ~pair,
  data = regression_sample 
)

pd_model3 <- feols(
  delta ~ boundary + boundary_margin + spread | pair,
  vcov = ~pair,
  data = regression_sample 
)

pd_model4 <- feols(
  delta ~ boundary + boundary_business + spread | pair,
  vcov = ~pair,
  data = regression_sample 
)

pd_model5 <- feols(
  delta ~ boundary + balance + spread | pair,
  vcov = ~pair,
  data = regression_sample
)

pd_model6 <- feols(
  delta ~  spotvola + latency_median + latency_variance_std + balance + spread | pair,
  vcov = ~pair,
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
