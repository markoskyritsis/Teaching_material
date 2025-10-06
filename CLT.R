#We have a population
pop <- rnorm(100000, 60, 30)

#Simulation
sampleMeans <- c()
sampleSDs <- c()
for (i in 1:10000) {
  #take a little sample each time, let's say of 30
  samp <- sample(pop, 30)
  sampleMeans[i] <- mean(samp)
  sampleSDs[i] <- sd(samp)
}

#What is the mean of the means?
mean(sampleMeans)
sd(sampleMeans)

successRate <- 0
sdSampDist <- sd(sampleMeans)
#Is the population mean inside each sample? Let's use 2SD
for (i in 1:length(sampleMeans)) {
  if (sampleMeans[i] - 1.96*sdSampDist <= mean(pop) & sampleMeans[i] + 1.96*sdSampDist >= mean(pop)) {
    successRate <- successRate + 1
  }
}

#so question is, how do we get sdSampDist?
#take any sample
dist <- c()
ses <- c()
for (i in 1:10000) {
  se <- sampleSDs[i] / sqrt(30)
  dist[i] <- abs(se - sdSampDist)
  ses[i] <- se
}

mean(dist)

successRate <- 0

#Is the population mean inside each sample? Let's use 2SD
for (i in 1:length(sampleMeans)) {
  if (sampleMeans[i] - 1.96*ses[i] <= mean(pop) & sampleMeans[i] + 1.96*ses[i] >= mean(pop)) {
    successRate <- successRate + 1
  }
}
