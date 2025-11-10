#Generate two populations
pop1 <- rnorm(100000, 100, 30)
pop2 <- rnorm(100000, 100, 30)


#We wouldn't expect two random samples from these to correlate
#here is proof
cor.test(pop1,pop2)




#Okay back to the uncorrelated samples
pop1 <- rnorm(100000, 100, 40)
pop2 <- rnorm(100000, 100, 40)

#An empty array
sample_r <- c()

#We will stick to 30 people per sample
for (i in 1:100000) {
  sample1 <- sample(pop,30)
  sample2 <- sample(pop2,30)
  #Add the correlation coefficient, which we "expect" to be zero
  sample_r[i] <- cor(sample1,sample2)
}


#So if we have two samples that are correlated
#We can easily generate populations that do correlate:
pop <- rnorm(30,100,30)
#Add some noise
#Noise strength (play with this)
noise <- 150
pop_cor <- pop + rnorm(length(pop),0,noise)
# You can see how this affects the second population
head(pop)
head(pop_cor)
#The more noise, the less correlation 
cor.test(pop,pop_cor)
actual_r <- cor(pop,pop_cor)

#What is the chance of getting the observed difference
#under our simulation?
P_value = sum(abs(sample_r) >= abs(actual_r)) / 100000
print(paste0("pvalue is ",P_value))

#As we see, the sampling distribution generates
#The same p-value as the t-test (more or less)

