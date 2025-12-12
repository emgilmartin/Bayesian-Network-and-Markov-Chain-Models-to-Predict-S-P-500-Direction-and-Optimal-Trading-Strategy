library(dplyr)
par(mfrow = c(1,1))
#align the signal with the price data
signal_mod <- predicted_signal_mod
signal_mod[is.na(signal_mod)] <- 0

price  <- sp500_df$Close[(length(sp500_df$Close)-length(signal_mod)):length(sp500_df$Close)]
dates  <- sp500_df$Date[(length(sp500_df$Close)-length(signal_mod)):length(sp500_df$Close)]
n <- length(price)

# make sure signal has the same length
signal_mod <- tail(signal_mod, n)


# Daily price difference

price_diff <- c(NA, diff(price))     # length n
price_diff <- price_diff[-1]         # length n-1
dates_valid <- dates[-1]             # aligned with price_diff


#find where the signal flips and flag them
signal_lag <- dplyr::lag(signal_mod, 1, default = 0)

flip_flag <- (signal_mod != signal_lag) & (signal_mod != 0)
flip_indices <- which(flip_flag)

if (length(flip_indices) == 0)
  stop("No trades detected.")

first_trade_idx <- flip_indices[1]


#vector of trading position
position <- dplyr::lag(signal_mod, 1, default = 0)

#Daily returns
strategy_ret <- position * price_diff
cum_strategy <- cumsum(strategy_ret)

# Align to first trade date
cum_strategy_ft <- cum_strategy[first_trade_idx:(n-1)]
dates_ft <- dates_valid[first_trade_idx:(n-1)]

# Calculate buy and hold strategy

bh_price <- price[first_trade_idx:n]
bh_return <- bh_price - bh_price[1]
bh_return <- bh_return[-1]


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


# Plot
trade_dates <- dates[flip_indices]
trade_dates_ft <- trade_dates[trade_dates >= dates_ft[1]]
plot(dates_ft, cum_strategy_ft, type = "l", col = "blue", lwd = 2,
     main = "Cumulative Returns of Original BN Model",
     xlab = "Date", ylab = "Cumulative Return ($)",
     ylim = c(min(cum_strategy_ft, bh_return),
              max(cum_strategy_ft, bh_return)))
lines(dates_ft, bh_return, col = "black", lwd = 2)
#abline(v = trade_dates_ft, col="red", lty=3)

legend("topleft", legend = c("BN Strategy", "Buy & Hold"),
       col = c("blue", "black"), lwd = 2)
grid()


# Results

cat("Trades:", num_trades, "\n")
cat("Avg Profit per Trade:", round(avg_profit, 2), "\n")
cat("Win Rate:", round(win_rate * 100, 1), "%\n")
cat("Final Cumulative P/L:", round(tail(cum_strategy_ft, 1), 2), "\n")
