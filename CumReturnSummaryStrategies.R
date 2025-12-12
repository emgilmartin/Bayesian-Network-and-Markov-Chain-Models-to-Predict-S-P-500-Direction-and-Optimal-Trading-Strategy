library(dplyr)

# Backtest function
backtest_strategy <- function(signal_mod, price, dates, plot_results = TRUE, strategy_name = "Strategy") {
  
  # Replace NAs with 0
  signal_mod[is.na(signal_mod)] <- 0
  n <- length(price)
  
  # make sure vectors are same length
  signal_mod <- tail(signal_mod, n)
  dates <- tail(dates, n)
  
  # Daily price difference
  price_diff <- diff(price)
  dates_valid <- dates[-1]  
  
  # Position vector lagged by 1 to initiate trade next day
  position <- dplyr::lag(signal_mod, 1, default = 0)[-1] 
  
  # Strategy daily returns
  strategy_ret <- position * price_diff
  cum_strategy <- cumsum(strategy_ret)
  
  # find signal flips
  signal_lag <- dplyr::lag(signal_mod, 1, default = 0)
  flip_flag <- (signal_mod != signal_lag) & (signal_mod != 0)
  flip_indices <- which(flip_flag)
  
  if (length(flip_indices) == 0) {
    warning("No trades detected.")
    return(NULL)
  }
  
  # Start from first trade
  first_trade_idx <- flip_indices[1]
  strategy_ret_ft <- strategy_ret[first_trade_idx:length(strategy_ret)]
  cum_strategy_ft <- cumsum(strategy_ret_ft)
  dates_ft <- dates_valid[first_trade_idx:length(dates_valid)]
  dates_ft <- as.Date(dates_ft)
  
  # Buy & hold from first trade
  bh_price <- price[first_trade_idx:length(price)]
  bh_return <- bh_price - bh_price[1]
  bh_return <- bh_return[-1]  # drop first to align with strategy
  
  # PnL for each trade
  trade_idx_ret <- flip_indices - 1
  trade_idx_ret <- trade_idx_ret[trade_idx_ret >= 1]
  num_trades <- length(trade_idx_ret)
  per_trade_profit <- numeric(num_trades)
  
  for (i in 1:num_trades) {
    start_idx <- trade_idx_ret[i]
    end_idx <- ifelse(i < num_trades,
                      trade_idx_ret[i+1] - 1,
                      length(strategy_ret))
    per_trade_profit[i] <- sum(strategy_ret[start_idx:end_idx])
  }
  
  avg_profit <- mean(per_trade_profit)
  win_rate <- mean(per_trade_profit > 0)
  
  # Sharpe ratio
  sharpe_ratio <- function(r) {
    r <- r[is.finite(r)]
    if (length(r) < 2) return(0)
    s <- sd(r)
    if (is.na(s) || s == 0) return(0)
    mean(r) / s * sqrt(252)
  }
  
  sr <- sharpe_ratio(strategy_ret)
  
  # Plot
  if (plot_results) {
    plot(dates_ft, cum_strategy_ft, type = "l", col = "blue", lwd = 2,
         main = paste("Cumulative Returns -", strategy_name),
         xlab = "Date", ylab = "Cumulative Return ($)",
         ylim = range(c(cum_strategy_ft, bh_return), finite = TRUE))
    lines(dates_ft, bh_return, col = "black", lwd = 2)
    abline(v = dates_valid[flip_indices], col="red", lty=3)
    legend("topleft", legend = c(strategy_name, "Buy & Hold"),
           col = c("blue", "black"), lwd = 2)
    grid()
  }
  
  # Results
  list(
    strategy_ret = strategy_ret,
    cum_strategy = cum_strategy,
    cum_strategy_ft = cum_strategy_ft,
    per_trade_profit = per_trade_profit,
    num_trades = num_trades,
    avg_profit = avg_profit,
    win_rate = win_rate,
    sharpe = sr,
    first_trade_idx = first_trade_idx,
    dates_ft = dates_ft,
    bh_return = bh_return
  )
}


# Prepare price and dates
price <- sp500_df$Close[(length(sp500_df$Close)-length(p_up_vector)+1):length(sp500_df$Close)]
dates <- sp500_df$Date[(length(sp500_df$Close)-length(p_up_vector)+1):length(sp500_df$Close)]

# Run backtest for each trade strategy
results_conservative <- backtest_strategy(signal_conservative, price, dates, plot_results = FALSE)
results_balanced     <- backtest_strategy(signal_balanced, price, dates, plot_results = FALSE)
results_aggressive   <- backtest_strategy(signal_aggressive, price, dates, plot_results = FALSE)

# Plot each strategy
# Align to first trade date
first_trade_idx <- min(results_conservative$first_trade_idx,
                       results_balanced$first_trade_idx,
                       results_aggressive$first_trade_idx)

dates_plot <- results_conservative$dates_ft
dates <- as.Date(dates)
all_dates <- as.Date(all_dates)

plot(NULL,
     xlim = range(c(results_conservative$dates_ft,
                    results_balanced$dates_ft,
                    results_aggressive$dates_ft)),
     ylim = range(c(results_conservative$cum_strategy_ft,
                    results_balanced$cum_strategy_ft,
                    results_aggressive$cum_strategy_ft, results_conservative$bh_return)),
     xlab = "Date",
     ylab = "Cumulative Return",
     main = "M2 Model Strategy Comparison", xaxt = "n")

axis.Date(1, at = pretty(all_dates, n = 6))

# Add each line separately
lines(results_conservative$dates_ft,
      results_conservative$cum_strategy_ft,
      col = "blue", lwd = 2)

lines(results_balanced$dates_ft,
      results_balanced$cum_strategy_ft,
      col = "green", lwd = 2)

lines(results_aggressive$dates_ft,
      results_aggressive$cum_strategy_ft,
      col = "red", lwd = 2)
lines(dates_plot, results_conservative$bh_return, col="black", lwd=2)

legend("topleft", legend=c("Conservative", "Balanced", "Aggressive", "Buy & Hold"),
       col=c("blue", "green", "red", "black"), lwd=2)
grid()


# Results Table
strategy_names <- c("Conservative", "Balanced", "Aggressive")
results_list <- list(results_conservative, results_balanced, results_aggressive)

summary_table <- data.frame(
  Strategy = strategy_names,
  Num_Trades = sapply(results_list, function(x) x$num_trades),
  Avg_Profit = sapply(results_list, function(x) round(x$avg_profit, 2)),
  Win_Rate   = sapply(results_list, function(x) round(x$win_rate * 100, 1)),
  Final_PL   = sapply(results_list, function(x) round(tail(x$cum_strategy_ft, 1), 2)),
  Sharpe     = sapply(results_list, function(x) round(x$sharpe, 3))
)

print(summary_table)


