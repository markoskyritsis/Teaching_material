library(ggplot2)
library(boot)
library(dplyr)

# 1. Generate Skewed Data (Log-Normal)
# -----------------------------------
set.seed(55) # Good seed for visible skew
n <- 30
data_vec <- rlnorm(n, meanlog = 1, sdlog = 1) # Skewed right
sample_mean <- mean(data_vec)

# 2. Calculate Intervals
# -----------------------------------

# A. T-Interval (Symmetric)
t_res <- t.test(data_vec)
ci_t <- data.frame(
  Method = "T-Statistic (Parametric)",
  Lower = t_res$conf.int[1],
  Upper = t_res$conf.int[2],
  Mean  = sample_mean,
  Y_Pos = -0.5 # Position on the Y-axis (below plot)
)

# B. Bootstrap BCa (Asymmetric)
get_mean <- function(data, indices) mean(data[indices])
boot_obj <- boot(data_vec, get_mean, R = 2000)
boot_ci <- boot.ci(boot_obj, type = "bca")

ci_boot <- data.frame(
  Method = "Bootstrap (BCa)",
  Lower = boot_ci$bca[4],
  Upper = boot_ci$bca[5],
  Mean  = sample_mean,
  Y_Pos = -1.5 # Position on the Y-axis (lower down)
)

# Combine for plotting
ci_df <- rbind(ci_t, ci_boot)

# 3. Create the Pedagogical Plot
# -----------------------------------
ggplot(data.frame(x = data_vec), aes(x = x)) +
  # A. The Data Distribution
  geom_density(fill = "gray90", color = "gray50", alpha = 0.5) +
  geom_rug(alpha = 0.5) + # Little tick marks showing actual data points
  
  # B. The Intervals (Horizontal Bars)
  geom_segment(data = ci_df, 
               aes(x = Lower, xend = Upper, y = Y_Pos, yend = Y_Pos, color = Method), 
               size = 2, arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
  
  # C. The Sample Mean (Vertical Line)
  geom_vline(xintercept = sample_mean, linetype = "dashed", size = 1) +
  annotate("text", x = sample_mean, y = 0.05, label = "Sample Mean", angle = 90, vjust = -1) +
  
  # D. Styling
  scale_color_manual(values = c("T-Statistic (Parametric)" = "blue", "Bootstrap (BCa)" = "red")) +
  labs(title = "Symmetry vs. Reality: 95% Confidence Intervals",
       subtitle = paste0("Sample Size n=", n, " (Log-Normal Distribution)"),
       x = "Observed Value",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(), # Hide y-axis numbers for cleaner look
        axis.title.y = element_blank()) +
  
  # Adjust limits to make room for the bars below
  coord_cartesian(ylim = c(-2.5, NA), clip = "off")