library(MASS)
library(expm)
### Adaptation of MarkovChainExamples.r to use kaggle data
# load data
# Define the base path
path <- "~/Downloads/MarkovChainKaggleData"  
# Build the full path to the CSV file using file.path() and load first test data set
#Dados.Intraday gives minute by minute prices for four days
#Data.Daily gives daily closing price

#file <- file.path(path, "Data.Daily", "AAPL_D.csv")
# Extract ticker from file name
#file_name <- basename(file)  
#ticker <- sub("_D\\.csv", "", file_name)  # remove "_D.csv"
# Read the CSV
#df <- read.csv(file)
# View the first few rows
#head(df)
data <- sp500_df$Close[(length(sp500_df$Close)-999):length(sp500_df$Close)]
#data = sp500_df$Close[length(data)-365:length(data)]
#data = c(45.00, 44.49, 43.44, 40.17, 41.05, 41.53, 41.36, 40.68, 40.46, 38.42)

#transform data
returns <- round((data[-1] - data[-length(data)]) / data[-length(data)],3)
head(data)
head(returns)
plot(returns, main = paste("Percent Price Change for", ticker), type = 'l', xlab = "Day", ylab = "Percent Price change")
plot(data, type = 'l', main = paste("Daily closing Price for", ticker), xlab = "Day", ylab = "Price ($)")
# Use standard deviation alpha0 to get static bin midpoints
alpha0 <- sd(returns)

# State labels
bins <- c("sM", "sm", "s1", "s2", "s3", "s4", "sZ")
mid_f_values <- c(-3, -2, -1, 0, 1, 2, 3)  # center positions of bins
names(mid_f_values) <- bins

#calculating Markov Chain for when speculator plays more risk (P_M)
# Compute midpoints using alpha0  This is needed to get the different alphas from each beginning state
midpoints_alpha0 <- alpha0 * mid_f_values

# Define shift function f(s) for main states
f_bin <- c(sm = 2, s1 = 1, s2 = 0, s3 = -1, s4 = -2)
main_states <- names(f_bin)
numStates <- length(main_states)

# Calculate adaptive alpha for each main state using consecutive state
adaptive_alpha <- numeric(numStates)
names(adaptive_alpha) <- main_states

for (s in main_states) {
  mi <- midpoints_alpha0[s]
  
  # Choose a consecutive class
  # Use the next neighbor unless s == s4 in which case use the previous
  idx <- which(names(midpoints_alpha0) == s)
  if (s == "s4") {
    sj <- names(midpoints_alpha0)[idx - 1]
  } else {
    sj <- names(midpoints_alpha0)[idx + 1]
  }
  
  mj <- midpoints_alpha0[sj]
  adaptive_alpha[s] <- abs((mj - mi) / (mi + 100)) * 100
}

#Create transition matrix
p_matrix <- matrix(0, nrow = numStates, ncol = length(bins))
rownames(p_matrix) <- paste0("State_", main_states)
colnames(p_matrix) <- bins

for (s in 1:numStates) {
  state <- main_states[s]
  shift <- f_bin[state]
  alpha <- adaptive_alpha[state]
  
  #Initialize counts
  counts <- setNames(rep(0, length(bins)), bins)
  
  for (i in 1:length(returns)) {
    value <- returns[i]
    
    # Define thresholds using shifted alpha
    th_sM         <- (-2.5 + shift) * alpha
    th_sm_lower   <- (-2.5 + shift) * alpha
    th_sm_upper   <- (-1.5 + shift) * alpha
    th_s1_lower   <- (-1.5 + shift) * alpha
    th_s1_upper   <- (-0.5 + shift) * alpha
    th_s2_lower   <- (-0.5 + shift) * alpha
    th_s2_upper   <- (0.5  + shift) * alpha
    th_s3_lower   <- (0.5  + shift) * alpha
    th_s3_upper   <- (1.5  + shift) * alpha
    th_s4_lower   <- (1.5  + shift) * alpha
    th_s4_upper   <- (2.5  + shift) * alpha
    th_sZ         <- (2.5  + shift) * alpha
    
    # Bin classification
    if (value <= th_sM) {
      counts["sM"] <- counts["sM"] + 1
    } else if (value <= th_sm_upper) {
      counts["sm"] <- counts["sm"] + 1
    } else if (value <= th_s1_upper) {
      counts["s1"] <- counts["s1"] + 1
    } else if (value <= th_s2_upper) {
      counts["s2"] <- counts["s2"] + 1
    } else if (value <= th_s3_upper) {
      counts["s3"] <- counts["s3"] + 1
    } else if (value <= th_s4_upper) {
      counts["s4"] <- counts["s4"] + 1
    } else {
      counts["sZ"] <- counts["sZ"] + 1
    }
  }
  
  # Convert to relative frequencies
  p_matrix[s, ] <- counts / sum(counts)
}

# Add absorbing rows for sM and sZ
topRow <- c(1, 0, 0, 0, 0, 0, 0)
bottomRow <- c(0, 0, 0, 0, 0, 0, 1)
pM_matrix <- rbind(topRow, p_matrix, bottomRow)
rownames(pM_matrix)[1] <- "State_sM"
rownames(pM_matrix)[nrow(pM_matrix)] <- "State_sZ"

# Display final result
print("Market Transition Matrix: Speculator Plays More Risk")
print(fractions(pM_matrix))

#-----------------------------------------------------------------------------
#calculating Markov Chain for when speculator plays less risk (P_L)
# State labels
bins <- c("sm", "s1", "s2", "s3", "s4", "sZ")
mid_f_values <- c(-2, -1, 0, 1, 2, 3)  # center positions of bins
names(mid_f_values) <- bins[2:7]

#calculating Markov Chain for when speculator plays more risk (P_M)
# calculate midpoints using alpha0
midpoints_alpha0 <- alpha0 * mid_f_values

#Define shift function f(s) for main states
f_bin <- c(s1 = 1, s2 = 0, s3 = -1, s4 = -2)
main_states <- names(f_bin)
numStates <- length(main_states)

#Calculate adaptive alpha for each main state using consecutive state
adaptive_alpha <- numeric(numStates)
names(adaptive_alpha) <- main_states

for (s in main_states) {
  mi <- midpoints_alpha0[s]
  
  # Choose a consecutive class
  # Use the next neighbor unless s == s4 in which case use the previous
  idx <- which(names(midpoints_alpha0) == s)
  if (s == "s4") {
    sj <- names(midpoints_alpha0)[idx - 1]
  } else {
    sj <- names(midpoints_alpha0)[idx + 1]
  }
  
  mj <- midpoints_alpha0[sj]
  adaptive_alpha[s] <- abs((mj - mi) / (mi + 100)) * 100
}

#Create transition matrix
p_matrix <- matrix(0, nrow = numStates, ncol = length(bins))
rownames(p_matrix) <- paste0("State_", main_states)
colnames(p_matrix) <- bins

for (s in 1:numStates) {
  state <- main_states[s]
  shift <- f_bin[state]
  alpha <- adaptive_alpha[state]
  
  #Initialize counts
  counts <- setNames(rep(0, length(bins)), bins)
  
  for (i in 1:length(returns)) {
    value <- returns[i]
    
    # Define thresholds using new alpha
    th_sm   <- (-1.5 + shift) * alpha
    th_s1_lower   <- (-1.5 + shift) * alpha
    th_s1_upper   <- (-0.5 + shift) * alpha
    th_s2_lower   <- (-0.5 + shift) * alpha
    th_s2_upper   <- (0.5  + shift) * alpha
    th_s3_lower   <- (0.5  + shift) * alpha
    th_s3_upper   <- (1.5  + shift) * alpha
    th_s4_lower   <- (1.5  + shift) * alpha
    th_s4_upper   <- (2.5  + shift) * alpha
    th_sZ         <- (2.5  + shift) * alpha
    
    # Bin classification
    if (value <= th_sm) {
      counts["sm"] <- counts["sm"] + 1
    } else if (value <= th_s1_upper) {
      counts["s1"] <- counts["s1"] + 1
    } else if (value <= th_s2_upper) {
      counts["s2"] <- counts["s2"] + 1
    } else if (value <= th_s3_upper) {
      counts["s3"] <- counts["s3"] + 1
    } else if (value <= th_s4_upper) {
      counts["s4"] <- counts["s4"] + 1
    } else {
      counts["sZ"] <- counts["sZ"] + 1
    }
  }
  
  # Convert to relative frequencies
  p_matrix[s, ] <- counts / sum(counts)
}

# Add absorbing rows for sM and sZ
topRow <- c(1, 0, 0, 0, 0, 0)
bottomRow <- c(0, 0, 0, 0, 0, 1)
pL_matrix <- rbind(topRow, p_matrix, bottomRow)
rownames(pL_matrix)[1] <- "State_sM"
rownames(pL_matrix)[nrow(pL_matrix)] <- "State_sZ"

# Display final result
print("Market Transition Matrix: Speculator Plays Less Risk")
print(fractions(pL_matrix))

## Get long run probabilities------------------------------------------------
n = 10000  #starts to not play at 24
# More risk:
#estimate p2 by multiplying initial state vector by Matrix^n
pi0M <- t(c(0,0,0,1,0,0,0)) #t transposes the vector and we start at s2
pinM <- pi0M %*% (pM_matrix %^% n)
print(pinM) # this is long run estimation
#estimate p0 by doing one iteration
pi1M <- pi0M %*% pM_matrix # this is one iteration estimation
print(fractions(pi1M))

# probabilities for long run if trader plays more risk
p2M_longRun <- pinM[1]
p0M_longRun <- pinM[7]
p1M_longRun <- 1 - p2M_longRun - p0M_longRun
print(c(p0M_longRun, p1M_longRun, p2M_longRun))

#probabilites for one iteration if trader plays more risk
p2M_oneIt <- pi1M[1]
p0M_oneIt <- pi1M[7]
p1M_oneIt <- 1 - p2M_oneIt - p0M_oneIt
print(c(p0M_oneIt, p1M_oneIt, p2M_oneIt))

# Less risk:
pi0L <- t(c(0,0,1,0,0,0)) # start at s2
pinL <- pi0L %*% (pL_matrix %^% n)
print(pinL)
pi1L <- pi0L %*% pL_matrix
print(fractions(pi1L))

#probabilities for long run if trader chooses less risk
p2L_longRun <- pinL[1]
p0L_longRun <- pinL[6]
p1L_longRun <- 1 - p2L_longRun - p0L_longRun
print(c(p0L_longRun, p1L_longRun, p2L_longRun))

#probabilities for one iteration if trader chooses less risk
p2L_oneIt <- pi1L[1]
p0L_oneIt <- pi1L[6]
p1L_oneIt <- 1 - p2L_oneIt - p0L_oneIt
print(fractions(c(p0L_oneIt, p1L_oneIt, p2L_oneIt)))

#------------------------------------------------------------------------------
#compare with probability triangle to see which method better
x <- 2 * alpha0 # less risk loss
y <- 3 * alpha0 # more risk loss
w <- 3* alpha0 # profit

# 1-iteration
if (p2M_oneIt < w/(w+y) && p1M_oneIt/p2M_oneIt > (y-x)/(w+x)){
  print("For one iteration, play more risk")
} else if(p1L_oneIt + p2L_oneIt< w/(w+x) && p1L_oneIt/p2L_oneIt > (y-x)/(w+x)){
  print("For one iteration, play less risk")
} else {
  print("For one iteration, do not play")
}

# long run
if (p2M_longRun < w/(w+y) && p1M_longRun/p2M_longRun > (y-x)/(w+x)){
  print("For long run, play more risk")
} else if(p1L_longRun + p2L_longRun< w/(w+x) && p1L_longRun/p2L_longRun > (y-x)/(w+x)){
  print("For long run, play less risk")
} else {
  print("For long run, do not play")
}


#Monte carlo random walk simulation

simulate_market_paths <- function(transition_matrix, midpoints, start_state, start_price,
                                  n_steps = 200, num_sims = 200, sigma_rw = 0.01) {
  
  # Use rownames as the states
  state_names <- rownames(transition_matrix)
  
  # Ensure matrix is square and column names match row names
  colnames(transition_matrix) <- state_names
  
  # Replace NAs with 0 and make sure rows add to 1
  transition_matrix[is.na(transition_matrix)] <- 0
  transition_matrix <- transition_matrix / rowSums(transition_matrix)
  
  # check midpoints exist for all states
  for (s in state_names) {
    if (!(s %in% names(midpoints))) midpoints[s] <- 0
  }
  
  #Initialize matrix to store simulated paths
  paths <- matrix(NA, nrow = n_steps, ncol = num_sims)
  
  for (i in 1:num_sims) {
    returns <- numeric(n_steps)
    current_state <- start_state
    
    for (t in 1:n_steps) {
      base_return <- midpoints[current_state]
      
      # Random-walk perturbation
      returns[t] <- base_return + rnorm(1, mean = 0, sd = sigma_rw)
      
      # Sample next state using transition probabilities
      current_state <- sample(state_names, 1, prob = transition_matrix[current_state, ])
    }
    
    # use returns to get cumulative price path
    paths[, i] <- start_price * cumprod(1 + returns)
  }
  
  return(paths)
}

start_price <- sp500_df$Close[length(sp500_df$Close)]

# define midpoints
midpoints <- c(
  "State_s1" = adaptive_alpha[1],
  "State_s2" = adaptive_alpha[2],
  "State_s3" = adaptive_alpha[3],
  "State_s4" = adaptive_alpha[4],
  "State_sM" = 0,
  "State_sm" = 0,
  "State_sZ" = 0
)

# start from state 2
start_state <- "State_s2"

# Simulate
num_sims <- 300
n_steps <- 24
sigma_rw <- 0.01

paths <- simulate_market_paths(pL_matrix, midpoints, start_state, start_price,
                               n_steps, num_sims, sigma_rw)

# Plot
colors <- rainbow(num_sims, alpha = 0.3)
matplot(paths, type="l", lty=1, col=colors,
        main="Monte Carlo Price Paths",
        xlab="Time Step", ylab="Price")

paths_M <- simulate_market_paths(pM_matrix, midpoints, start_state, start_price,
                                 n_steps, num_sims, sigma_rw)
paths_L <- simulate_market_paths(pL_matrix, midpoints, start_state, start_price,
                                 n_steps, num_sims, sigma_rw)
matplot(paths_M, type="l", lty=1, col=rgb(1,0,0,0.4),
        main="Monte Carlo Price Paths",
        xlab="Time Step", ylab="Price")
matlines(paths_L, type = 'l',lty=1, col = rgb(0,0,1,0.3))
legend("topleft",                         
       legend = c("More Risk", "Less Risk"),  
       col = c(rgb(1,0,0,0.8), rgb(0,0,1,0.8)), 
       lty = 1,                            
       cex = 0.8)                          

#quantify performance of each

paths_M <- simulate_market_paths(pM_matrix, midpoints, start_state, start_price,
                                 n_steps, num_sims, sigma_rw)
paths_L <- simulate_market_paths(pL_matrix, midpoints, start_state, start_price,
                                 n_steps, num_sims, sigma_rw)

# final prices
final_M <- paths_M[n_steps, ]  # last row = final price for each simulation
final_L <- paths_L[n_steps, ]

# Total returns
returns_M <- final_M / start_price - 1
returns_L <- final_L / start_price - 1

# Summary statistics
summary(returns_M)
summary(returns_L)

mean_return_M <- mean(returns_M)
mean_return_L <- mean(returns_L)

cat("Average return More Risk:", round(mean_return_M*100,2), "%\n")
cat("Average return Less Risk:", round(mean_return_L*100,2), "%\n")

returns <- returns_M
#calculate sharep ratio
sharpe_ratio <- function(returns) {
  mu <- mean(returns)
  sigma <- sd(returns)
  if (sigma == 0) return(NA)
  return(mu / sigma)
}

# get metrics for each strategy
perf_metrics <- function(returns, label) {
  mean_ret  <- mean(returns)
  median_ret <- median(returns)
  sd_ret    <- sd(returns)
  sharpe    <- sharpe_ratio(returns)
  prob_loss <- mean(returns < 0)
  
  cat("\n-----------------------------\n")
  cat(" Strategy:", label, "\n")
  cat("-----------------------------\n")
  cat("Mean Return:         ", round(mean_ret, 4), "\n")
  cat("Median Return:       ", round(median_ret, 4), "\n")
  cat("Std Dev (Volatility):", round(sd_ret, 4), "\n")
  cat("Sharpe Ratio:        ", round(sharpe, 4), "\n")
  cat("Probability of Loss: ", round(prob_loss*100, 2), "%\n")
  
  return(
    data.frame(
      Strategy = label,
      Mean_Return = mean_ret,
      Median_Return = median_ret,
      SD = sd_ret,
      Sharpe = sharpe,
      Prob_Loss = prob_loss
    )
  )
}

#run the function
results_M <- perf_metrics(returns_M, "More Risk")
results_L <- perf_metrics(returns_L, "Less Risk")

# make results into a table
results_table <- rbind(results_M, results_L)
print(results_table)

#Longe run calcs
P = pM_matrix

#define absorbing and transient states
absorbing <- c(1,7)
transient <- setdiff(1:nrow(P), absorbing)

Q <- P[transient, transient]
R <- P[transient, absorbing]

#calculate fundamental matrix
I <- diag(length(transient))
N <- solve(I - Q)

#calculate absorption probabilities
B <- N %*% R

p2M_longRun <- B[3,1]  # major adversity
p0M_longRun <- B[3,2]  # zero adversity
p1M_longRun <- 1 - p2M_longRun - p0M_longRun #minor adversity

