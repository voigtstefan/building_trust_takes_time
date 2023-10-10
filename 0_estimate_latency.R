# DO NOT RUN duration model estimation  -----
# Final output is available on repo latency_duration_model_parameters.rds
library(tidyverse)
library(lubridate)

source("_config.R")

# DO NOT RUN read and prepare mempool data ------------------------------------------------
transactions <- list.files("data/transactions", full.names = TRUE)

# DO NOT RUN estimate parameters of survival regressions -----------------------------
survival_regression <- function(tau, X, distribution = 'exponential', 
                                standardize = FALSE, hessian = FALSE) {
  ## this function computes MLE survival regression estimates for two models
  ## 1. exponential model: tau ~ exp(lambda), where lambda = exp(-X'gamma) 
  ## 2. gamma model: tau ~ Gamma(alpha, beta) where beta = exp(-X'gamma) 
  
  sample_clean <- na.omit(cbind(tau, 1, X))
  tau <- sample_clean[, 1]
  X <- as.matrix(sample_clean[, -1])
  
  if(ncol(X) > 1 & standardize) {
    X[, -1] <- scale(X[, -1]) # standardize all variables
  }
  
  f.exp <- function(gamma, X, tau){
    ## log-likelihood of the exponential model
    lambda <- exp(-X%*%gamma)
    logL <- sum(log(lambda) - lambda*tau, na.omit=TRUE)
    return(-logL)
  }
  
  f.gamma <- function(gamma, X, tau){
    ## log-likelihood of the gamma model
    alpha <- exp(gamma[1])
    beta <- exp(-X%*%gamma[2:(1+ncol(X))])
    logL <- sum(alpha*log(beta)-lgamma(alpha)+(alpha-1)*log(tau)-beta*tau, na.omit = TRUE)
    return(-logL)
  }
  
  ## optimization procedure  
  if(distribution == 'exponential'){
    fit_rest <- optim(0, 
                      lower = 0, 
                      upper = 100,
                      f.exp, 
                      X = as.matrix(X[,1]), 
                      hessian = hessian,
                      tau = tau, 
                      control = list(maxit = 50000),
                      method = 'Brent')
    
    fit_unrest <- optim(rep(0, ncol(X)), 
                        f.exp, 
                        X = X, 
                        hessian = hessian,
                        tau = tau, 
                        control = list(maxit = 50000)) 
    
    residuals_rest <- tau - exp(X[, 1]%*%as.matrix(fit_rest$par))
    residuals_unrest <- tau - exp(X%*%as.matrix(fit_unrest$par))
    
    out <- list(
      optimizer_convergence_rest = fit_rest$convergence,
      optimizer_convergence_unrest = fit_unrest$convergence,
      beta_rest = fit_rest$par,
      beta_unrest = fit_unrest$par,
      llik_rest = -fit_rest$value,
      llik_unrest = -fit_unrest$value,
      distribution = distribution,
      sample_mean = mean(tau,na.rm=TRUE),
      chisq = 2*(fit_unrest$value-fit_rest$value),
      n = nrow(X),
      mse_rest = mean(residuals_rest^2),
      mse_unrest = mean(residuals_unrest^2)
    )
    
  } else if(distribution == 'gamma') {
    
    fit_rest <- optim(c(0, 0), 
                      f.gamma, 
                      X = as.matrix(X[,1]), 
                      tau = tau, 
                      hessian = hessian, 
                      control = list(maxit = 50000))
    
    fit_unrest <- optim(rep(0, ncol(X)+1), 
                        f.gamma, 
                        X = X, 
                        tau = tau, 
                        hessian = hessian, 
                        control = list(maxit = 50000))
    
    pred_rest <- exp(fit_rest$par[1]) / exp(-X[,1]%*%as.matrix(fit_rest$par[-1]))
    pred_unrest <- exp(fit_unrest$par[1]) / exp(-X%*%fit_unrest$par[-1])
    
    residuals_rest <- tau - pred_rest
    residuals_unrest <- tau - pred_unrest
    
    out <- list(
      optimizer_convergence_rest = fit_rest$convergence,
      optimizer_convergence_unrest = fit_unrest$convergence,
      alpha_rest = exp(fit_rest$par[1]),
      alpha_unrest = exp(fit_unrest$par[1]),
      beta_rest = fit_rest$par[-1],
      beta_unrest = fit_unrest$par[-1],
      llik_rest = -fit_rest$value,
      llik_unrest = -fit_unrest$value,
      distribution = distribution,
      sample_mean = mean(tau,na.rm=TRUE),
      chisq = 2*(fit_unrest$value-fit_rest$value),
      n = nrow(X),
      mse_rest = mean(residuals_rest^2),
      mse_unrest = mean(residuals_unrest^2)
    )
  }
  
  return(out)  
  
}

estimate_parameters <- function(j) {
  ## load data
  data <- read_rds(paste0(transactions[j]))
  if (!is.na(transactions[j+1])) { # no future day for last day in sample
    data_future <- read_rds(paste0(transactions[j+1]))
  }
  
  ## define covariates
  data <- data %>% 
    filter(unconfirmed_tx > 0) 
  X <- data %>%
    select(tx_fee_per_byte, unconfirmed_tx) %>%
    mutate(unconfirmed_tx = log(unconfirmed_tx))
  
  ## estimate model
  fit_exp <- survival_regression(tau = data$tx_latency, 
                                 X = X, 
                                 distribution = "exponential",
                                 standardize = FALSE)
  
  fit_gamma <- survival_regression(tau = data$tx_latency, 
                                   X = X, 
                                   distribution = "gamma",
                                   standardize = FALSE)
  
  ## evaluate out-of-sample predictions (except for last day in sample)
  if (exists("data_future")) {
    data_future <- data_future %>% 
      filter(unconfirmed_tx > 0) 
    X_oos <- data_future %>% 
      select(tx_fee_per_byte, unconfirmed_tx) %>%
      mutate(unconfirmed_tx = log(unconfirmed_tx))
    X_oos <- as.matrix(cbind(1, X_oos))
    tau_oos <- data_future$tx_latency
    
    mse_oos_exp_rest <- mean((tau_oos - exp(X_oos[, 1] %*% as.matrix(fit_exp$beta_rest)))^2)
    mse_oos_exp_unrest <- mean((tau_oos - exp(X_oos %*% as.matrix(fit_exp$beta_unrest)))^2)
    
    mse_oos_gamma_rest <- mean((tau_oos - fit_gamma$alpha_rest / exp(-X_oos[, 1] %*% as.matrix(fit_gamma$beta_rest)))^2)
    mse_oos_gamma_unrest <- mean((tau_oos - fit_gamma$alpha_unrest / exp(-X_oos %*% fit_gamma$beta_unrest))^2)
  } else {
    mse_oos_exp_rest <- as.numeric(NA)
    mse_oos_exp_unrest <- as.numeric(NA)
    
    mse_oos_gamma_rest <- as.numeric(NA)
    mse_oos_gamma_unrest <- as.numeric(NA)
  }
  
  ## collect estimation results in one table
  out_exp_rest <- tibble(model = "exponential",
                         type = "restricted",
                         parameters = c("intercept", "loglikeli", "chisq"),
                         values = c(fit_exp$beta_rest, fit_exp$llik_rest, fit_exp$chisq),
                         convergence = fit_exp$optimizer_convergence_rest,
                         mse_ins = fit_exp$mse_rest,
                         mse_oos = mse_oos_exp_rest)
  
  out_exp_unrest <- tibble(model = "exponential",
                           type = "unrestricted",
                           parameters = c("intercept", "beta_1", "beta_2", "loglikeli"),
                           values = c(fit_exp$beta_unrest, fit_exp$llik_unrest),
                           convergence = fit_exp$optimizer_convergence_unrest,
                           mse_ins = fit_exp$mse_unrest,
                           mse_oos = mse_oos_exp_unrest)
  
  out_gamma_rest <- tibble(model = "gamma",
                           type = "restricted",
                           parameters = c("alpha", "intercept", "loglikeli", "chisq"),
                           values = c(fit_gamma$alpha_rest, fit_gamma$beta_rest, fit_gamma$llik_rest, fit_gamma$chisq),
                           convergence = fit_gamma$optimizer_convergence_rest,
                           mse_ins = fit_gamma$mse_rest,
                           mse_oos = mse_oos_gamma_rest)
  
  out_gamma_unrest <- tibble(model = "gamma",
                             type = "unrestricted",
                             parameters = c("alpha","intercept", "beta_1", "beta_2", "loglikeli"),
                             values = c(fit_gamma$alpha_unrest, fit_gamma$beta_unrest, fit_gamma$llik_unrest),
                             convergence = fit_gamma$optimizer_convergence_unrest,
                             mse_ins = fit_gamma$mse_unrest,
                             mse_oos = mse_oos_gamma_unrest)
  
  out <- bind_rows(out_exp_rest, out_exp_unrest, 
                   out_gamma_rest, out_gamma_unrest) %>%
    mutate(date = as.Date(gsub("[^0-9.]", "",  transactions[j]), format = "%Y%m%d"))

  write_rds(out, gsub("transactions", "latency", transactions[j]))
  
  cat(as.character(Sys.time()), ": Done!", "\n")
}

## parallel execution
j <- as.integer(Sys.getenv("SGE_TASK_ID"))
print(transactions[j])
estimate_parameters(j)

# DO NOT RUN Output available on repo latency_duration_model_parameters.rds ----
latencies <- list.files("data/latency", full.names = TRUE) %>%
  map(read_rds) %>%
  reduce(bind_rows)

write_rds(latencies, "data/latency_duration_model_parameters.rds")
