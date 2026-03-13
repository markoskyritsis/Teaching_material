# 1. The Setup (Comparing 3 Marketing Strategies)
sales_data <- data.frame(
  Sales = c(rnorm(30, 100, 15), rnorm(30, 125, 15), rnorm(30, 105, 15)),
  Campaign = factor(rep(c("Standard", "Influencer", "Email"), each = 30))
)

# 2. The ANOVA (The "General Audit")
model <- aov(Sales ~ Campaign, data = sales_data)
summary(model)

# 3. The TukeyHSD (The "Specific Comparison")
# This automatically adjusts for the 'Multiple Comparison' problem 
# we discussed in the lecture!
tukey_results <- TukeyHSD(model)
print(tukey_results)

# 4. Visualising the "Confidence Intervals"
plot(tukey_results)