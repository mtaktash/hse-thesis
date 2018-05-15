# Functions for data loading and model evaluation

load_stock_data <- function(filename){
  
  price <- read.zoo(filename, 
                    header = TRUE, 
                    sep = ",",
                    format = "%Y-%m-%d"
  )
  
  log_price <- log(price)
  returns <- diff(log_price)
  squared_returns <- returns ** 2
  stock_data <- merge(price, log_price, returns, squared_returns)
  stock_data <- na.omit(stock_data)
  return(stock_data)
  
}

printvec <- function(vec){
  cat(paste(names(vec), vec, sep = ": ", collapse = "\n"))
}


# Extract coefficients from sliding window estimation 
get_coefs <- function(roll, include_external = FALSE, external = NULL){
  
  coefs <- data.frame(
    date = as.Date(integer(), origin = "1970-01-01"),
    alpha1 = double(),
    alpha1_std = double(),
    beta1 = double(),
    beta1_std = double(),
    stringsAsFactors = FALSE) 
  
  for (s in seq(from = 1, to = length(coef(roll)), by = 1)){
    
    coefs[s, 'index'] <- s
    
    coefs[s, 'alpha1'] <- coef(roll)[s][[1]]$coef['alpha1', 1]
    coefs[s, 'alpha1_std'] <- coef(roll)[s][[1]]$coef['alpha1', 2]
    
    coefs[s, 'beta1'] <- coef(roll)[s][[1]]$coef['beta1', 1]
    coefs[s, 'beta1_std'] <- coef(roll)[s][[1]]$coef['beta1', 2]
    
  }
  
  coefs[, 'alpha1beta1'] <- (coefs$alpha1 + coefs$beta1)
  coefs[, 'alpha1beta1_std'] <- coefs$alpha1_std + coefs$beta1_std
  
  if (include_external == TRUE){
    for (s in seq(from = 1, to = length(coef(roll)), by = 1)){
      for (ext in external){
        coefs[s, ext] <- coef(roll)[s][[1]]$coef[ext, 1]
        coefs[s, sprintf("%s_std", ext)] <- coef(roll)[s][[1]]$coef[ext, 2]
      }
    }
  }
  
  rownames(coefs) <- coefs$index
  return(coefs)
  
}


# Plot sliding window estimation coefficient with confidence intervals
plot_coef <- function(coefs, value, value_std, title, ylims = c(0, 1)){
  
  return(ggplot(coefs, aes(index, value)) + 
           geom_line() +
           geom_line(aes(index, value + value_std), linetype = "dashed") +
           geom_line(aes(index, value - value_std), linetype = "dashed") +
           ylim(ylims) +
           ggtitle(title) +
           geom_hline(yintercept = 1, linetype = "dotted") +
           geom_hline(yintercept = 0, linetype = "dotted"))
}


# Plot GARCH model sliding window estimation coefficiens in one grid
plot_garch_coefs <- function(coefs, company_name){
  
  alpha1 <- plot_coef(coefs, coefs$alpha1, coefs$alpha1_std, paste('alpha1', company_name))
  beta1 <- plot_coef(coefs, coefs$beta1, coefs$beta1_std, paste('beta1', company_name) )
  alpha1beta1 <- plot_coef(coefs, coefs$alpha1beta1, coefs$alpha1beta1_std, paste('alpha1 + beta1', company_name), c(0, 1.3))
  
  print(plot_grid(alpha1, beta1, alpha1beta1, nrow = 1))
  
}

fit_roll <- function(spec, stock_data, refit.every = 20, refit.window = "recursive", window = 1000){
  
  roll <- ugarchroll(spec, stock_data$returns, 
                     refit.every = refit.every, 
                     n.start = window,
                     refit.window = refit.window,  
                     window.size = window,
                     fit.control = list(rec.init = 0.7),
                     solver = "hybrid", 
                     keep.coef = TRUE)
  resume(roll)
  return(roll)
  
}


# Forecast evaluation criterion
get_R_h <- function(spec, data, h = 10, window = 1000, external.forecasts = NULL){
  
  external = list(mregfor = NULL, vregfor = external.forecasts)
  R_h <- data.frame(value = double())
  
  modelfit <- ugarchfit(spec, data = data[1:window], fit.control = list(rec.init = 0.7)) 
  spec <- getspec(modelfit)
  setfixed(spec) <- as.list(coef(modelfit))
  
  forecast <- ugarchforecast(spec, n.ahead = h, 
                             n.roll = (length(data) - window), 
                             data = data, 
                             out.sample = (length(data) - window), 
                             external.forecasts = external)
  forecast_sigma <- as.data.frame(sigma(forecast))
  true_sigma <- data[window:length(data)]
  true_sigma <- as.numeric(true_sigma)

  for (t in seq(length(forecast_sigma) - h)){
    vol_true <- true_sigma[t:(t + h - 1)] ** 2
    vol_pred <- forecast_sigma[,t]
    R_h[t, 'value'] <- (sum(vol_true) - sum(vol_pred)) ** 2
  }
  return(R_h) 
}


