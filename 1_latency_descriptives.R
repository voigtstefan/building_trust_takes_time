library(tidyverse)
library(lubridate)
library(xtable)

source("_config.R")

# summarize latency parametrization results -------------------------------
## load daily latency parameter estimates
latencies <- read_rds("data/latency_duration_model_parameters.rds")

## create parameter estimates summary table
stats_params <- latencies %>% 
  filter(parameters != "loglikeli" & parameters != "chisq") %>% 
  mutate(values = case_when(parameters == "intercept" ~ values,
                            parameters == "alpha" ~ values,
                            parameters == "beta_1" ~ 247*values,
                            parameters == "beta_2" ~ values)) %>%
  group_by(model, type, parameters) %>% 
  summarise (m = round(mean(values, na.rm = TRUE), 2),
             zint = paste0('{[', 
                          round(quantile(values, 0.10, na.rm=TRUE), 3), ',',
                          round(quantile(values, 0.90, na.rm=TRUE), 3), ']}')) %>%
  gather(X, values, -parameters, -model, -type) %>% 
  unite(temp, model, type, sep = ' ') %>%
  mutate(parameters = factor(parameters, levels = c("intercept", "alpha", "beta_1", "beta_2"))) %>% 
  arrange(temp, parameters) %>% 
  spread(temp, values) %>% 
  select(-X) %>%
  mutate(parameters = c('Intercept', '', '$\\alpha$', '', 'Fee per Byte', '', 
                        'Mempool Size', '')) %>%
  rename(' ' = 'parameters')

## stats_log_likelihood
stats_likelihood <- latencies %>%
  filter(parameters == "loglikeli") %>%
  select(date, model, type, values) %>% 
  spread(type, values) %>%
  mutate(m = -2*(restricted-unrestricted)) %>% 
  group_by(model) %>% 
  summarise(m = as.character(round(100*mean(m > 20.515), 2))) %>%
  mutate(model =paste(model, "restricted")) %>%
  bind_rows(data.frame(model = c("exponential unrestricted", "gamma unrestricted"), m = c(NA, NA))) %>%
  mutate(parameters = 'LR (Covariates)' ) %>%
  spread(model, m) %>% 
  rename(' ' = 'parameters')

## stats_log_likelihood_across_models
stats_likelihood_models <- latencies %>%
  filter(parameters =="loglikeli") %>%
  select(date, model, type, values) %>%
  filter(type=="unrestricted") %>%  
  spread(model, values) %>%
  mutate(m = -2*(exponential-gamma)) %>% 
  summarise(m = as.character(round(100*mean(m > 20.515), 2))) %>%
  mutate(model ="exponential restricted") %>%
  bind_rows(data.frame(model = c("exponential unrestricted", 
                                 "gamma restricted", "gamma unrestricted"), m = NA)) %>%
  spread(model, m) %>% 
  mutate(' ' = 'LR (Gamma vs. Exponential)')

## predictive accuracy
predictive_accuracy_models <- latencies %>%
  filter(mse_ins < quantile(mse_ins, 0.90), 
         mse_oos < quantile(mse_oos, 0.90, na.rm = T))%>%
  select(-(parameters:convergence)) %>%
  group_by(date, model, type) %>% 
  distinct(mse_ins, mse_oos) %>%
  group_by(model, type) %>%
  summarise(mse_ins = round(mean(sqrt(mse_ins)),2),
            mse_oos = round(mean(sqrt(mse_oos)),2)) %>%
  ungroup() %>% 
  unite(col="model", model, type, sep=" ") %>%
  gather(' ', value,-model) %>%
  spread(model, value) %>%
  mutate(' ' = c('MSPE (In-Sample)', 'MSPE (Out-of-Sample)')) %>%
  mutate_all(list(as.character))

## combine everything and print to latex table
stats <- bind_rows(stats_params, stats_likelihood, 
                   stats_likelihood_models,
                   predictive_accuracy_models) %>%
  mutate_all(list(as.character))

stats <- rbind(c(" ", "W/o Covariates", "W/ Covariates",
                 "W/o Covariates", "W/ Covariates"), stats)

print(xtable(stats, align = c("r","l","c","c","c","c")), 
      booktabs = TRUE, 
      include.colnames = FALSE,
      include.rownames = FALSE, 
      NA.string = " ",
      floating = FALSE,
      file = 'output/tab_model_fit.tex',
      sanitize.text.function = function(x){x},
      add.to.row = list(pos = as.list(c(0, 1, 9, 11)),
                        command = c("& \\multicolumn{2}{c}{Exponential} & \\multicolumn{2}{c}{Gamma}  \\\\ \\cmidrule(lr){2-3}\\cmidrule(lr){4-5} ", 
                                     "\\midrule ","\\midrule ", "\\midrule ")))

