
library(MASS)
library(expm)
# Load the reticulate library which lets uss use python packages
library(reticulate)
library(TTR)  # for EMA
library(bnlearn)
library(tidyr)
library(dplyr)
library(fredr)
library(crayon)
library(zoo)
library(ggplot2)

#Get M2 data:
# FRED KEY: 4ede27e3f7a2b460fe3c80be6ebaaf89 % gives access to federal reserve data
fredr_set_key("4ede27e3f7a2b460fe3c80be6ebaaf89")
# Get M2 money supply from Jan 1, 2019 to the present
m2_data <- fredr(
  series_id = "M2SL",
  observation_start = as.Date("2019-01-01"),
  sort_order = "desc"
)

# get growth rate to define binary states for node
m2_data <- m2_data %>%
  arrange(date) %>%
  mutate(
    m2_growth = 100 * (log(value) - lag(log(value)))   # % monthly growth
  )
tail(m2_data)

m2_data <- m2_data %>%
  rename(Date = date, Value = value) #Make sure column names match so the data can be combined

# Keep only Date and m2_growth and forward fill for missing days
m2_data_daily <- m2_data %>%
  dplyr::select(Date, m2_growth) %>%
  # create a sequence of daily dates
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  # forward-fill missing M2 values
  mutate(m2_growth = na.locf(m2_growth, na.rm = FALSE))

# Import the yfinance to get S&P 500 data
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
#sp500_df$Date <- as.Date(sp500_df$Date)
#colnames(sp500_df) <- c("Close", "High", "Low", "Open", "Volume", "Date")
vix_df <- py_to_r(vix)
vix_df$Date <- as.Date(rownames(vix_df))
colnames(vix_df) <- c("Close", "High", "Low", "Open", "Volume", "Date")
t10y_df <- py_to_r(t10y)
t10y_df$Date <- as.Date(rownames(t10y_df))
colnames(t10y_df) <- c("Close", "High", "Low", "Open", "Volume", "Date")
head(sp500_df)
head(vix_df)
head(t10y_df)

# calculate log returns for S&P 500 Close prices
sp500_df$log_return <- c(NA, diff(log(sp500_df$Close)))

# Remove NA values from returns for volatility calc
returns <- na.omit(sp500_df$log_return)

# Calculate daily volatility (std dev)
volatility <- sd(returns)

# Annualize volatility assuming 252 trading days in a year
annualized_vol <- volatility * sqrt(252)

print(paste("Daily Volatility:", round(volatility, 5)))
print(paste("Annualized Volatility:", round(annualized_vol, 5)))

# calculate signal_trend
n <- 200  # decay window in days (example) larger means smoother, smaller reacts more to volatility
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


# Lag rolling_sd by 1 (to align)
sp500_df$lagged_sd <- lag(sp500_df$rolling_sd, 1)

# S&P500 returns sign 1 if return >= 0 else 0
sp500_df$SPX500_ret <- ifelse(c(NA, diff(log(sp500_df$Close))) >= 0, 1, 0)

# calculate returns for other variables
  # For volatility return compute daily volatility (rolling_sd) and then its daily return
sp500_df$vol_ret <- ifelse(c(NA, diff(sp500_df$rolling_sd)) >= 0, 1, 0)
t10y_df$T10y_ret <- ifelse(c(NA, diff(log(t10y_df$Close))) >= 0, 1, 0)
vix_df$VIX_ret   <- ifelse(c(NA, diff(log(vix_df$Close))) >= 0, 1, 0)

# make data frame with discretized variables  and remove NAs
#this first part makes sure that the dates don't get misaligned and that an entire day is removed if we don't have all the data.
m2_data_daily <- m2_data_daily %>%
  mutate(
    m2_growth_cat = ifelse(m2_growth >= 0, "up", "down") %>% as.factor()
  )
merged_data <- sp500_df %>%
  dplyr::select(Date, SPX500_ret, signal_trend, vol_ret) %>%
  inner_join(vix_df %>% dplyr::select(Date, VIX_ret), by = "Date") %>%
  inner_join(t10y_df %>% dplyr::select(Date, T10y_ret), by = "Date") %>%
  inner_join(m2_data_daily %>% dplyr::select(Date, m2_growth_cat), by = "Date") %>%  # use categorical only
  mutate(SPX500_ret_shift = lead(SPX500_ret)) %>%
  na.omit()

# make data to feed into bnlearn hav to drop date column for it to work
bn_data <- merged_data %>%
  dplyr::select(-Date) %>%  
  mutate(
    signal_trend = as.factor(signal_trend),
    vol_ret = as.factor(vol_ret),
    VIX_ret = as.factor(VIX_ret),
    T10y_ret = as.factor(T10y_ret),
    SPX500_ret_shift = as.factor(SPX500_ret_shift),
    m2_growth_cat = as.factor(m2_growth_cat),  
    SPX500_ret = as.factor(SPX500_ret)
  )
#vector to store model predictions
v <- nrow(bn_data)
predicted_signal_mod <- rep(NA, v)
library(bnlearn)

start_day <- 300

#define what arcs we want in the structure
wl <- data.frame(
  from = c("vol_ret", "signal_trend", "signal_trend", "signal_trend", 
           "SPX500_ret", "SPX500_ret", "VIX_ret", "T10y_ret", "m2_growth_cat", "m2_growth_cat", "m2_growth_cat"),
  to   = c("T10y_ret", "T10y_ret", "VIX_ret", "SPX500_ret_shift", 
           "SPX500_ret_shift", "VIX_ret", "SPX500_ret_shift", "SPX500_ret_shift", "SPX500_ret_shift", "VIX_ret", "T10y_ret")
)
#define what arcs we don't want in the structure
bl <- data.frame(
  from = c("signal_trend", "vol_ret"),
  to = c("m2_growth_cat", "m2_growth_cat")
)

#create the train data and test data
for (i in start_day:(v - 1)) {
  train_data <- bn_data[1:i, ]
  test_data  <- bn_data[i + 1, , drop = FALSE]
  
  # Match factor levels between train and test data
  test_data <- lapply(names(test_data), function(col) {
    factor(test_data[[col]], levels = levels(train_data[[col]]))
  }) %>% as.data.frame()
  colnames(test_data) <- names(train_data)
  
  # Learn the structure and fit Bayesian Network usign hill climbing algorithm
  model <- hc(train_data, whitelist = wl, blacklist = bl)
  #learn wihtout whitelist
  #model <- hc(train_data)
  fit <- bn.fit(model, data = train_data)

  
  # Try to predict probability
  prob <- tryCatch({
    predict(fit, node = "SPX500_ret_shift", data = test_data, prob = TRUE)
  }, error = function(e) NULL)
  
  prob_attr <- attr(prob, "prob")
  
  #this part calculates predicted_signal_mod
  if (!is.null(prob_attr) && "1" %in% rownames(prob_attr)) {
    p_up <- prob_attr["1", 1]
    predicted_signal_mod[i + 1] <- ifelse(p_up > 0.5, 1, -1)  # take position
  } else {
    predicted_signal_mod[i + 1] <- 0  # 0 = no position (flat)
  }
}

# Learn structure from the full dataset
full_model <- hc(bn_data, whitelist = wl, blacklist = bl)

# fit parameters too
full_fit <- bn.fit(full_model, bn_data)

# Plot structure
graphviz.plot(full_model, main = "Bayesian Network Structure With M2")
arcs(full_model)

## Creating plots with signals 
library(ggplot2)
library(zoo)
par(mfrow = c(1,1))
#Align predicted signal with price data
# If predicted_signal_mod has no Date column, assume it matches the tail of sp500_df
n_price <- nrow(sp500_df)
n_signal <- length(predicted_signal_mod)

# add NA to begin to match price history
if(n_signal < n_price){
  pred_signal <- c(rep(NA, n_price - n_signal), predicted_signal_mod)
} else {
  pred_signal <- predicted_signal_mod
}

# Forward-fill and set remaining NAs to 0
pred_signal <- zoo::na.locf(pred_signal, na.rm = FALSE)
pred_signal[is.na(pred_signal)] <- 0

# Combine with sp500_df
sp500_df_signal <- sp500_df %>%
  mutate(pred_signal = pred_signal)

# Create groups for consecutive signals
sp500_df_signal <- sp500_df_signal %>%
  mutate(
    trend_group = cumsum(c(1, diff(pred_signal) != 0)),
    ymin_up = ifelse(pred_signal == 1, min(Close, na.rm = TRUE), NA),
    ymax_up = ifelse(pred_signal == 1, max(Close, na.rm = TRUE), NA),
    ymin_down = ifelse(pred_signal == -1, min(Close, na.rm = TRUE), NA),
    ymax_down = ifelse(pred_signal == -1, max(Close, na.rm = TRUE), NA)
  )

# Plot price with signals
ggplot(sp500_df_signal, aes(x = Date)) +
  geom_line(aes(y = Close), color = "black", size = 0.7) +
  geom_ribbon(aes(ymin = ymin_up, ymax = ymax_up, group = trend_group), 
              fill = "green", alpha = 0.2, na.rm = TRUE) +
  geom_ribbon(aes(ymin = ymin_down, ymax = ymax_down, group = trend_group), 
              fill = "red", alpha = 0.2, na.rm = TRUE) +
  labs(title = "S&P500 Closing Price with Predicted Trend Signal",
       y = "Closing Price", x = "Date") +
  theme_minimal()


