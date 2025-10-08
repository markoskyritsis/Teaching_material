#Does the t-test work?

#Type I error rate should be ~ 5%
#One population
pop <- rnorm(100000, 100, 30)

#The t-test should NOT return a significant result
#Let's see how many times this is correct, say out of
#1000

failureRate <- 0
for (i in 1:1000) {
  #Sample twice from the same population
  samp1 <- sample(pop, 20)
  samp2 <- sample(pop, 20)
  #Get the p-value
  p_value <- t.test(samp1,samp2,var.equal = T)$p.value
  if (p_value < 0.05) {
    #Type 1 error detected. Increment counter
    failureRate <- failureRate + 1
  }
}
print(paste0("The Type 1 error rate is: ",failureRate/1000))
#If we run this multiple times it should centre around 5%

#In fact, here is how to smooth it out over time:

failureRateArray <- c()
for (j in 1:100) {
  failureRate <- 0
  for (i in 1:1000) {
    samp1 <- sample(pop, 20)
    samp2 <- sample(pop, 20)
    p_value <- t.test(samp1,samp2,var.equal = T)$p.value
    if (p_value < 0.05) {
      failureRate <- failureRate + 1
    }
  }
  failureRateArray[j] <- failureRate/1000
}
#This should be very close to 0.05, which is expected type I
mean(failureRateArray)

#Question for students, what happens to Type I if:
#the population distribution is not normal?
#The standard deviation is large or small?
#The sample size is very small or very large?
#Is type I error rate stable?


