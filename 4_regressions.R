library(tidyverse)
library(fixest)

source("_config.R")
regression_sample <- read_csv("data/regression_sample.csv")

# Prepare regression sample -----------------------------------------------
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
    c(inflows, delta, delta_regional, spotvola, spread,
      balance,
      boundary),
    ~ trim(., 0.01)
  )) |>
  drop_na(delta, spotvola, spread, boundary, median_latency, sd_latency, inflows) |> 
  # gdax removed margin end of february 2018
  mutate(margin_trading = if_else(sell_side == "gdax" & ts >= "2018-02-28", FALSE, as.logical(margin_trading))) |> 
  # replace missing number of confirmations
  mutate(
    aa_rating = rating_categorial == "AA",
    no_of_confirmations = replace_na(no_of_confirmations, 3),
    balance = replace_na(balance, 0),
    inflows = inflows * btc_price / 100000,
    boundary_margin = margin_trading * boundary,
    boundary_business = business_accounts * boundary,
    log_inflows = log(1 + inflows),
    log_balance = log(1 + balance),
    latency_variance = sd_latency ^ 2,
    latency_variance_std = scale(latency_variance)
  )
  
# Summary statistics ------------------------------------------------------
regression_sample_prepared |> 
  select(
    delta, delta_regional, inflows, spotvola, median_latency, sd_latency, boundary, 
    balance, balance, spread, margin_trading, 
    business_accounts
  ) |> 
  pivot_longer(cols = everything()) |> 
  drop_na() |>
  group_by(name) |>
  summarize(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    q05 = quantile(value, 0.05),
    q50 = quantile(value, 0.50),
    q95 = quantile(value, 0.95),
    max = max(value),
    n = n()
  )
  
# label variables
dictionary <- c(
  delta = "Price Differences (in %)",
  delta_regional = "Regional Price Differences (in %)",
  inflows = "Exchange Inflows (in 100k USD)",
  spotvola = "Spot Volatility (in %)",
  median_latency = "Latency Median (in Min)",
  sd_latency = "Latency (SD)",
  latency_variance_std = "Latency Variance (Standardized)",
  boundary = "Arbitrage Bound (in %)",
  boundary_margin = "Arbitrage Bound X Margin Trading",
  boundary_business = "Arbitrage Bound X Business Accounts",
  spread = "Spread (in %)",
  balance = "Inventory (in BTC)",
  net_flow = "Net flow (in BTC)",
  net_flow_usd = "Net flow (in USD)",
  no_of_confirmations = "Number of Confirmations",
  tether = "Tether",
  margin_trading = "Margin Trading",
  business_accounts = "Business Accounts",
  log_balance = "Inventory",
  sell_side = "Exchange Fixed Effects",
  log_inflows = "Log(Exchange Inflows)",
  aa_rating = "AA Rating"
)

# Price Differences and Sources of Price Risk -----------------------------
vcov <- "hetero"
pd_model1 <- feols(
  delta ~ boundary + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared
)

pd_model2 <- feols(
  delta ~ spotvola + median_latency + latency_variance_std + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared
)

pd_model3 <- feols(
  delta ~ boundary + boundary_margin + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared
)

pd_model4 <- feols(
  delta ~ boundary + boundary_business + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared
)

pd_model5 <- feols(
  delta ~ boundary + log_balance + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared
)

pd_model6 <- feols(
  delta ~  spotvola + median_latency + latency_variance_std + log_balance + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared 
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


  # Robustness check: regional splits ---------------------------------------
pd_model7 <- feols(
  delta_regional ~ boundary + log_balance + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared |> 
    filter(region == "USA")
)

pd_model8 <- feols(
  delta_regional ~  boundary + log_balance + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared |> 
    filter(region == "Europe")
)

pd_model9 <- feols(
  delta_regional ~  boundary + log_balance + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared
)

pd_model10 <- feols(
  delta ~  boundary*aa_rating + log_balance + spread | sell_side,
  vcov = vcov,
  data = regression_sample_prepared
)

etable(
  pd_model7, pd_model8, pd_model9, pd_model10, 
  coefstat = "tstat",
  dict = dictionary
)

etable(
  pd_model7, pd_model8, pd_model9, pd_model10, 
  coefstat = "tstat",
  dict = dictionary, 
  tex = TRUE, 
  style.tex = style.tex(line.top = "\\toprule", line.bottom = "\\bottomrule"), 
  headers = list("^:_:\\emph{Dependent Variable}" = c("Regional Price Differences (in %)", "Regional Price Differences (in %)", "Regional Price Differences (in %)", "Price Differences (in %)"),
                 "Region" = c("USA", "Europe", "All", "")), 
  fitstat=c('n'),
  depvar = FALSE,
  group=list("Controls: Inventory and Spread" = c("Spread", "Inventory")))


# Cross-Exchange Flows and Arbitrage Opportunities ------------------------
flows_model1 <- feols(
  inflows ~ spread | sell_side| delta ~ spotvola +  median_latency + sd_latency,
  vcov = vcov,
  data = regression_sample_prepared
)

flows_model2 <- feols(
  inflows ~  spread | sell_side| delta ~ boundary,
  vcov = vcov,
  data = regression_sample_prepared
)

flows_model3 <- feols(
  log_inflows ~ spread | sell_side| delta ~ spotvola +  median_latency + sd_latency,
  vcov = vcov,
  data = regression_sample_prepared
)

flows_model4 <- feols(
  log_inflows ~  spread | sell_side| delta ~ boundary,
  vcov = vcov,
  data = regression_sample_prepared
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



