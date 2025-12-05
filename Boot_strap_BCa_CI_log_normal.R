library(boot)

# 1. Setup Skewed Population (Log-Normal)
# ---------------------------------------------------------
#Change this every time you want to test a new one
set.seed(42)
# A strong skew to really test the BCa limits
pop <- rlnorm(100000, meanlog = 3, sdlog = 1) 
true_mean <- mean(pop)
n <- 20 

# 2. Define the Statistic Function (Required for 'boot')
# ---------------------------------------------------------
# The function must take 'data' and 'indices' as arguments
get_mean <- function(data, indices) {
  d <- data[indices]
  return(mean(d))
}

# 3. Simulation Loop
# ---------------------------------------------------------
ticker_t <- 0
ticker_perc <- 0
ticker_bca <- 0
negative_bounds_t <- 0

# We run 100 simulations (Increase to 1000 for a final paper)
for (i in 1:100) {
  samp <- sample(pop, n)
  
  # --- A. T-Test ---
  t_res <- t.test(samp)
  if (t_res$conf.int[1] <= true_mean & t_res$conf.int[2] >= true_mean) {
    ticker_t <- ticker_t + 1
  }
  if (t_res$conf.int[1] < 0) {
    negative_bounds_t <- negative_bounds_t + 1
  }
  
  # --- B. Bootstrap (The Industry Standard Way) ---
  # R=999 is standard (avoids ties in Monte Carlo p-values), 1999 is better for BCa
  # The R is the number of re-samples. 999 is standard, 1999 (+1 for our dataset = 2000) 
  #is a good trade-off
  boot_out <- boot(data = samp, statistic = get_mean, R = 1999)
  
  # Calculate Intervals (Percentile AND BCa)
  # boot.ci can fail if variance is 0 (rare in continuous data), so we wrap in tryCatch
  try({
    cis <- boot.ci(boot_out, type = c("perc", "bca"))
    
    # 1. Percentile Interval (Indices 4 and 5)
    lower_perc <- cis$percent[4]
    upper_perc <- cis$percent[5]
    if (lower_perc <= true_mean & upper_perc >= true_mean) {
      ticker_perc <- ticker_perc + 1
    }
    
    # 2. BCa Interval (Indices 4 and 5)
    # This is the "Gold Standard"
    lower_bca <- cis$bca[4]
    upper_bca <- cis$bca[5]
    if (lower_bca <= true_mean & upper_bca >= true_mean) {
      ticker_bca <- ticker_bca + 1
    }
  }, silent = TRUE)
}

# 4. Output
# ---------------------------------------------------------
print(paste0("T-Test Coverage: ", ticker_t, "%"))
print(paste0("T-Test Impossible Negative Bounds: ", negative_bounds_t))
print("----------------")
print(paste0("Bootstrap (Percentile) Coverage: ", ticker_perc, "%"))
print(paste0("Bootstrap (BCa) Coverage:        ", ticker_bca, "%"))


