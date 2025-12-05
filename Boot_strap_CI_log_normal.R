# 1. Setup Skewed Population (Log-Normal)
# ---------------------------------------------------------
set.seed(49)
# meanlog=3, sdlog=1 gives a long right tail (like income)
pop <- rlnorm(100000, meanlog = 3, sdlog = 1) 
#Visualise log-normal so you can see that the
#distribution is now skewed
plot(density(pop))


true_mean <- mean(pop) # Approx 33.1

# We'll use a small sample size where skewness hurts the most
n <- 15 

# Trackers
ticker_t <- 0
ticker_boot <- 0
t_negative_lower_bound <- 0 # Counting how often T-test gives impossible values

# 2. Simulation Loop
# ---------------------------------------------------------
for (i in 1:100) {
  samp <- sample(pop, n)
  
  # --- T-Statistic (Parametric) ---
  # Assumes the sampling distribution is Normal (CLT)
  t_res <- t.test(samp)
  ci_t_lower <- t_res$conf.int[1]
  ci_t_upper <- t_res$conf.int[2]
  
  if (ci_t_lower <= true_mean & ci_t_upper >= true_mean){
    ticker_t = ticker_t + 1    
  }
  
  # Check for "Impossible" intervals (Negative lower bound on positive data)
  if (ci_t_lower < 0) {
    t_negative_lower_bound = t_negative_lower_bound + 1
  }
  
  # --- Bootstrap (Percentile) ---
  # Does not assume symmetry
  boot_means <- replicate(5000, mean(sample(samp, n, replace = TRUE)))
  ci_boot <- quantile(boot_means, probs = c(0.025, 0.975))
  
  if (ci_boot[1] <= true_mean & ci_boot[2] >= true_mean){
    ticker_boot = ticker_boot + 1    
  }
}

# 3. Output
# ---------------------------------------------------------
print(paste0("True Population Mean: ", round(true_mean, 2)))
print(paste0("T-Stat Coverage: ", ticker_t, "%"))
print(paste0("Bootstrap Coverage: ", ticker_boot, "%"))
print(paste0("Times T-test predicted negative (impossible) values: ", t_negative_lower_bound))

#The t-test is robust, but fails on accuracy sometimes by
#producing negative lower bounds (which are impossible)
