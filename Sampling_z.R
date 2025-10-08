#We create two populations. Different means, same SD
pop1 <- rnorm(100000,100,25)
pop2 <- rnorm(100000,110,25)

#Sample twice
sample1 <- sample(pop1, 60)
sample2 <- sample(pop2, 60)

#Get the pooled variance and SD
pooled_var <- .5*(var(sample1) + var(sample2))
pooled_sd <- sqrt(pooled_var)

#Get the observed difference in means
diff_means <- mean(sample1) - mean(sample2)


#Construct a sampling distribution of mu1 = mu2
#Note that essentially we are saying that if mu1 = mu2
#then mu1 - mu2 = 0. So what are the chances that if
#this is true, we would get mu1 - m2 = observed difference?
mean_samples_diff <- c()
for (i in 1:100000) {
  mean_samples1 <- mean(rnorm(60, 0, pooled_sd))
  mean_samples2 <- mean(rnorm(60, 0, pooled_sd))
  mean_samples_diff[i] <- mean_samples1 - mean_samples2
}

#Note the normal distrubution again
plot(density(mean_samples_diff))

#Note. What is the SD of this sampling dist?
sd(mean_samples_diff)
#What would the SE be?
SE <- pooled_sd * sqrt(1/60 + 1/60)
#These should be close, showing the power of the SE formula

#What is the chance of getting the observed difference
#under our simulation?
P_value = sum(abs(mean_samples_diff) >= abs(diff_means)) / 100000
print(paste0("pvalue is ",P_value))

#Let's try with established tools now
library(BSDA)

#Since n > 30, we can use a z-test
z.test(sample1,sample2, alternative = "two.sided", sigma.x = pooled_sd, sigma.y = pooled_sd)

#But the t-test works just as well, since the t-distribution
#is quite similar after n = 30
t.test(sample1, sample2, var.equal = T,alternative = "two.sided")
