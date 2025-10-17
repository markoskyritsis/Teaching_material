#Multiple Comparisons

#One population, three samples
pop <- rnorm(100000,100,30)

#We expect that these samples will be the same, xbar1 - xbar2 = 0, 
#xbar2-xbar1 = 0, and so on. Let's do this 100 times
error_rate <- 0
for (i in 1:100) {
  samp1 <- sample(pop, 30)
  samp2 <- sample(pop, 30)
  samp3 <- sample(pop, 30)
  if (t.test(samp1,samp2,var.equal = T)$p.value < 0.05) {
    error_rate <- error_rate + 1
  }
  if (t.test(samp1,samp3,var.equal = T)$p.value < 0.05) {
    error_rate <- error_rate + 1
  }
  if (t.test(samp2,samp3,var.equal = T)$p.value < 0.05) {
    error_rate <- error_rate + 1
  }
}
#Let's print the result
print(error_rate)

#If you run the code multiple times (or nest it inside one more loop), you
#will note that the error rate will be around 15%
