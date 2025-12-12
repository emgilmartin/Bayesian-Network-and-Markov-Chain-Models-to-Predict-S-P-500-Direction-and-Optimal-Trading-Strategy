# Load the reticulate library which lets uss use python packages
library(reticulate)
library(TTR)  # for EMA
library(bnlearn)
library(tidyr)
library(dplyr)

reticulate::py_require("yfinance")
yfinance <- import("yfinance")

# Get S&P 500 (^GSPC) and VIX (^VIX) data and T10y (^TNX)
sp500 <- yfinance$download("^GSPC", start = "2019-01-01", end = "2025-10-01")
vix <- yfinance$download("^VIX", start = "2019-01-01", end = "2025-10-01")
t10y <- yfinance$download("^TNX", start = "2019-01-01", end = "2025-10-01")
#save as dataframes
sp500_df <- py_to_r(sp500)
#save dates to a column
sp500_df$Date <- as.Date(rownames(sp500_df))
colnames(sp500_df) <- c("Close", "High", "Low", "Open", "Volume", "Date")
vix_df <- py_to_r(vix)
vix_df$Date <- as.Date(rownames(vix_df))
colnames(vix_df) <- c("Close", "High", "Low", "Open", "Volume", "Date")
t10y_df <- py_to_r(t10y)
t10y_df$Date <- as.Date(rownames(t10y_df))
colnames(t10y_df) <- c("Close", "High", "Low", "Open", "Volume", "Date")
# View the data 
head(sp500_df)
head(vix_df)
head(t10y_df)
#calculate log returns for S&P 500 Close prices
sp500_df$log_return <- c(NA, diff(log(sp500_df$Close)))

# Remove NA values from returns for volatility calc
returns <- na.omit(sp500_df$log_return)

# Calculate daily volatility (std dev)
volatility <- sd(returns)

# Annualize volatility assuming 252 trading days in year
annualized_vol <- volatility * sqrt(252)

print(paste("Daily Volatility:", round(volatility, 5)))
print(paste("Annualized Volatility:", round(annualized_vol, 5)))

n <- 200  # decay window in days 
# Calculate EMA of prices
sp500_df$ema <- EMA(sp500_df$Close, n = n)
# Calculate rolling standard deviation of close prices or returns
sp500_df$rolling_sd <- runSD(sp500_df$Close, n = n)
# Calculate signal s_n(t)
sp500_df$signal <- (lag(sp500_df$Close, 1) - lag(sp500_df$ema, 1)) / lag(sp500_df$rolling_sd, 1)
# Calculate signal trend: sign(s_n(t))
sp500_df$signal_trend <- ifelse(sp500_df$signal > 0, 1, -1)
# Compute price differences (returns)
sp500_df$price_diff <- c(NA, diff(sp500_df$Close))

# Lag rolling_sd by 1 to align
sp500_df$lagged_sd <- lag(sp500_df$rolling_sd, 1)

# Example for SP500 returns sign (1 if return >= 0 else 0)
sp500_df$SPX500_ret <- ifelse(c(NA, diff(log(sp500_df$Close))) >= 0, 1, 0)

# calculate returns for our other variables
# For volatility return, compute daily volatility and then its daily return
sp500_df$vol_ret <- ifelse(c(NA, diff(sp500_df$rolling_sd)) >= 0, 1, 0)
t10y_df$T10y_ret <- ifelse(c(NA, diff(log(t10y_df$Close))) >= 0, 1, 0)
vix_df$VIX_ret   <- ifelse(c(NA, diff(log(vix_df$Close))) >= 0, 1, 0)

# Prepare data.frame with discretized variables  and remove NAs
#this first part makes sure that the dates don't get misaligned and that an entire day is removed if we don't have all the data.
merged_data <- sp500_df %>%
  dplyr::select(Date, SPX500_ret, signal_trend, vol_ret) %>%
  inner_join(vix_df %>% dplyr::select(Date, VIX_ret), by = "Date") %>%
  inner_join(t10y_df %>% dplyr::select(Date, T10y_ret), by = "Date") %>%
  mutate(SPX500_ret_shift = lead(SPX500_ret)) %>%
  na.omit()

bn_data <- merged_data %>%
  dplyr::select(-Date) %>%             # drop Date column
  mutate(across(everything(), as.factor))  # make all variables factors
#vector to store predictions
v <- nrow(bn_data)
predicted_signal_mod <- rep(NA, v)
library(bnlearn)

start_day <- 300  

wl <- data.frame(
  from = c("vol_ret", "signal_trend", "signal_trend", "signal_trend", 
           "SPX500_ret", "SPX500_ret", "VIX_ret", "T10y_ret"),
  to   = c("T10y_ret", "T10y_ret", "VIX_ret", "SPX500_ret_shift", 
           "SPX500_ret_shift", "VIX_ret", "SPX500_ret_shift", "SPX500_ret_shift")
)

for (i in start_day:(v - 1)) {
  train_data <- bn_data[1:i, ]
  test_data  <- bn_data[i + 1, , drop = FALSE]
  
  # Match factor levels between train and test data
  test_data <- lapply(names(test_data), function(col) {
    factor(test_data[[col]], levels = levels(train_data[[col]]))
  }) %>% as.data.frame()
  colnames(test_data) <- names(train_data)
  
  # Learn structure and fit Bayesian Network
  model <- hc(train_data, whitelist = wl)
  fit <- bn.fit(model, data = train_data)
  #learn wihtout whitelist
  # model <- hc(train_data)
  # fit <- bn.fit(model, data = train_data)
  
  # Try to predict probability
  prob <- tryCatch({
    predict(fit, node = "SPX500_ret_shift", data = test_data, prob = TRUE)
  }, error = function(e) NULL)
  
  prob_attr <- attr(prob, "prob")
  
  if (!is.null(prob_attr) && "1" %in% rownames(prob_attr)) {
    p_up <- prob_attr["1", 1]
    predicted_signal_mod[i + 1] <- ifelse(p_up > 0.5, 1, -1)  # take position
  } else {
    predicted_signal_mod[i + 1] <- 0
  }
}

# Learn structure from the full data set
full_model <- hc(bn_data, whitelist = wl)

#fit parameters
full_fit <- bn.fit(full_model, bn_data)

# Plot structure
graphviz.plot(full_model, main = "Original Bayesian Network Structure")
arcs(full_model)

