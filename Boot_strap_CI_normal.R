# 1. Setup Population
# ---------------------------------------------------------
set.seed(29) # This is so all of you get the same result
pop <- rnorm(100000, 100, 20)
true_mean <- mean(pop)
n <- 20 # We'll use a small sample size to highlight the differences


# 2. Z-Statistic
# ---------------------------------------------------------
# Expectation: This often gets < 95% because it assumes we know sigma 
# but uses sample SD, resulting in an interval that is too narrow.
# Generally works okay after n >= 60
ticker_z <- 0

for (i in 1:100) {
  samp <- sample(pop, n)
  standard_error <- sd(samp) / sqrt(n)
  
  # CRITICAL: qnorm uses 1.96, which is too small for n=20
  z_score <- qnorm(0.975) 
  
  ci_lower <- mean(samp) - (z_score * standard_error)
  ci_upper <- mean(samp) + (z_score * standard_error)
  
  if (ci_lower <= true_mean & ci_upper >= true_mean){
    ticker_z = ticker_z + 1    
  }
}


# 3. T-Statistic 
# ---------------------------------------------------------
# Expectation: Should be closer to 95% because t-dist is wider (fatter tails)
# to account for the uncertainty in estimating SD.
ticker_t <- 0

for (i in 1:100) {
  samp <- sample(pop, n)
  standard_error <- sd(samp) / sqrt(n)
  
  # CRITICAL: qt uses degrees of freedom (n-1) -> Value is approx 2.09
  t_score <- qt(0.975, df = n - 1) 
  
  ci_lower <- mean(samp) - (t_score * standard_error)
  ci_upper <- mean(samp) + (t_score * standard_error)
  
  if (ci_lower <= true_mean & ci_upper >= true_mean){
    ticker_t = ticker_t + 1    
  }
}


# 4. Bootstrap (Percentile Method)
# ---------------------------------------------------------
# Expectation: Should be approx 95%. Ideally 1000+ reps, but 
# calculating 2000 bootstraps inside a loop of 100 simulations 
# might take a few seconds.
ticker_boot <- 0

for (i in 1:100) {
  samp <- sample(pop, n)
  
  # Inner Loop: Create the Bootstrap Distribution
  # replicate() is a cleaner/faster version of a "for" loop in R
  boot_means <- replicate(5000, {
    boot_samp <- sample(samp, n, replace = TRUE)
    mean(boot_samp)
  })
  
  # Calculate CI using the 2.5th and 97.5th percentiles
  ci_boot <- quantile(boot_means, probs = c(0.025, 0.975))
  
  if (ci_boot[1] <= true_mean & ci_boot[2] >= true_mean){
    ticker_boot = ticker_boot + 1    
  }
}

# 5. Output Results
# ---------------------------------------------------------
print(paste0("Coverage Z-Stat: ", ticker_z, "% (Likely too low)"))
print(paste0("Coverage T-Stat: ", ticker_t, "% (Ideally ~95%)"))
print(paste0("Coverage Bootst: ", ticker_boot, "% (Ideally ~95%)"))