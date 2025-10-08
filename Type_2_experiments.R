#Type II experiment

#This time, we sample from two populations that are different
#Type II error = not finding an effect when one exists

#Play around with the means and the standard deviation and see what happens
pop1 <- rnorm(100000,100,30)
pop2 <- rnorm(100000,120,30)

#Clearly different. How many times does the t-test miss this
#difference, and what affects it?

failureRate <- 0 #Type II error rate
for (i in 1:1000) {
  #I would like students to play around with the sample size and see what 
  #happens
  samp1 <- sample(pop1,60)
  samp2 <- sample(pop2,60)
  p_value <- t.test(samp1,samp2, var.equal = T)$p.value
  if (p_value > 0.05) {
    #Type II error
    failureRate <- failureRate + 1
  }
}
print(paste0("Type II error rate for this simulation run was: ",failureRate/1000*100,"%"))

#You can see that Type II error rate is less stable than Type I. Harder to
#predict. Depends on sample size, sd of population, mean difference of population
#and so on.

#Question for students. You can't control the population but you can control
#sample size. How do you know that you have enough observations to minimise
#type II error. How much is an acceptable type II error?
