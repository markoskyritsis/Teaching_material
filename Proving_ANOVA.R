#Script that demonstrates the thought behind Analysis of Variance

#Let's start with a single population
pop <- rnorm(100000, 100, 30)

#If we were to sample, say, three times. We would expect that diff of all
#sample means is 0. However, this doesn't work reliably in the real world
#since there is always room for error (say 5%) and by comparing three 
#groups, or error rate ~ 15%. You can check my other script on multiple
#comparisons for proof of this.

#Let's think about this in a way that doesn't require MULTIPLE calculations
#if all three groups are the same, then the distance (variance) between
#the groups will be 0. Here is proof

#Note that I'm taking the var(means)

tempSample1 <- sample(pop, 30)
tempSample2 <- tempSample1
tempSample3 <- tempSample1
var(c(mean(tempSample1),mean(tempSample2),mean(tempSample3)))

#However, in the real world, there is sampling error. If we did sample
#three times from the same population it wouldn't exactly be zero. Here
#is proof
tempSample1 <- sample(pop, 30)
tempSample2 <- sample(pop, 30)
tempSample3 <- sample(pop, 30)
var(c(mean(tempSample1),mean(tempSample2),mean(tempSample3)))


#Okay, with that established, the next thought process is thus. If these
#are the same sample, then the mean average variance WITHIN the groups
#is the pooled sampling error (noise) of the samples.
#Here is what the "noise" or sampling error would look like for just
#one group
tempSample1 <- sample(pop, 30)
print(var(tempSample1))
#Let's plot it
plot(density(tempSample1))

#You can see that it isn't exactly 100. Obviously. We sampled from
#a population with mu = 100, but the x (the sample observations) 
#are not always 100. This is what we mean when we say "noise" in this
#context. We are talking about the natural variation

#Okay, let's pretend that the two other samples are exactly the same

#Note that I'm taking the mean(vars)
tempSample2 <- tempSample1
tempSample3 <- tempSample1
mean(c(var(tempSample1), var(tempSample2), var(tempSample3)))

#As you can see, the within group variance in this case is the same
#as the variance of one group. However, in the real world this 
#doesn't happen. Each sample will be different. Here is what
#would actually happen
tempSample2 <- sample(pop, 30)
tempSample3 <- sample(pop, 30)
mean(c(var(tempSample1), var(tempSample2), var(tempSample3)))

#If you run this multiple times, you will note that there is a degree
#of error variance. Of course, it makes sense since each sample is 
#unique

#Okay so far we checked the between group variance: var(means)
#we also checked the within group variance: mean(vars)

#What would happen if we were to take a ratio of these?

#some_statistic_theta <- var(means) / mean(vars)


#Well, in a world lacking noise (sampling error), we would get:

#some_statistic_theta <- 0 / some_number
#some_statistic_theta would be equal to ZERO

#For those that haven't done maths in a while. Dividing zero by any
#number would equal zero. 

#We know this won't happen in reality. But we can EXPECT theta to be zero

#I'm going to rename theta to F. This is actually the name we give
#this statistic

#So anyway, let's see what actually happens if you do this like 100000 times


F_ratios <- c()
for (i in 1:100000) {
  tempSample1 <- sample(pop, 30)
  tempSample2 <- sample(pop, 30)
  tempSample3 <- sample(pop, 30)
  
  #Numerator
  var_means <- var(c(mean(tempSample1),mean(tempSample2),mean(tempSample3)))
  #Denominator
  mean_vars <- mean(c(var(tempSample1), var(tempSample2), var(tempSample3)))

  F_ratios[i] <- var_means / mean_vars
}


#Let's start by plotting the F_distribution
plot(density(F_ratios))
#You'll note that this is a right-tailed distribution.

#This is the distribution for THREE groups that have a sample size
#of THIRTY. Make a note of that.

#Let's run a test. Let's create three samples with the same parameters:
#n = 30, and k = 3 (number of groups)
n <- 30
samp1 <- rnorm(n, mean = 100, sd = 30)
samp2 <- rnorm(n, mean = 120, sd = 30)
samp3 <- rnorm(n, mean = 90, sd = 30)

#I'm going to put this data in a data frame for visualisation purposes.
#Also we can compare our results to the official ANOVA test
Data <- c(samp1, samp2, samp3)
Group <- factor(c(rep("A", 30), rep("B", 30), rep("C", 30)))
ANOVA_Data <- data.frame(Data, Group)

#Numerator
var_means <- var(c(mean(samp1),mean(samp2),mean(samp3)))
#Denominator
mean_vars <- mean(c(var(samp1), var(samp2), var(samp3)))
#Here is our F_ratio
F_ratio <- var_means / mean_vars
print(F_ratio)

#Let's see what the probability of getting these variances are:
P_value = sum(abs(F_ratios) >= abs(F_ratio)) / 100000
print(P_value)
#Let's compare it to the probability using the F-statistic
summary(aov(ANOVA_Data$Data ~ ANOVA_Data$Group))

#You'll note that the p-value is the same, but the F_value is not.
#Why? (Note even if this next part doesn't make sense, don't worry)
#about it too much. The only thing that changes is how the F-statistic
#is calculated

#The scaling factor is missing from the calculation. the numerator 
#requires a critical scaling step to convert the variance of the means 
#into the actual sum of squares (and then the mean square). We multiply
#the numerator by the total sample size

#Numerator
var_means <- var(c(mean(samp1),mean(samp2),mean(samp3))) * 30
#Denominator
mean_vars <- mean(c(var(samp1), var(samp2), var(samp3))) 
#Here is our F_ratio
F_ratio <- var_means / mean_vars
print(F_ratio)

Data <- c(samp1, samp2, samp3)
Group <- factor(c(rep("A", 30), rep("B", 30), rep("C", 30)))
ANOVA_Data <- data.frame(Data, Group)
summary(aov(ANOVA_Data$Data ~ ANOVA_Data$Group))

#Now you'll note that the F-value matches




##############Example in Health Informatics###########
#We have two drugs and a placebo. Is there an effect of Drug on Blood marker?
#We want to reject the null hypothesis:
#HO: mu1 = mu2 = mu3

#So let's say, now that we have a situation where we have these three
#groups from an experiment:

#Here are three random groups. They happen to be a sample size of 30

DrugA <- rnorm(30, 100, 30)
DrugB <- rnorm(30, 120, 30)
DrugC <- rnorm(30, 105, 30)

#Put these in a dataframe
Data <- c(DrugA, DrugB, DrugC)
Group <- factor(c(rep("A", 30), rep("B", 30), rep("C", 30)))
ANOVA_Data <- data.frame(Data, Group)

#Let's use the sampling distribution we used
#Numerator
var_means <- var(c(mean(DrugA),mean(DrugB),mean(DrugC)))
#Denominator
mean_vars <- mean(c(var(DrugA), var(DrugB), var(DrugC)))
#Here is our UNSCALED F_ratio
F_ratio <- var_means / mean_vars

#Let's see what the probability of getting these variances are:
P_value = sum(abs(F_ratios) >= abs(F_ratio)) / 100000
print(P_value)
scaled_fvalue <- (var_means*30)/mean_vars
print(scaled_fvalue)

#Let's compare it to the probability using the F-statistic
summary(aov(ANOVA_Data$Data ~ ANOVA_Data$Group))

#You can run this multiple times, but you will notice that in each case
#the p-value matches the official F-test. 


#In conclusion, you can now see how the F-statistic works. 

#########################Homework##################################
#Use the F-distribution, as well as the Sampling distribution
#On the Iris dataset
?iris

#Here is the code to get each group's, say, Petal.Length
group1 <- iris[iris$Species == "setosa",]$Petal.Length
group2 <- iris[iris$Species == "versicolor",]$Petal.Length
group3 <- iris[iris$Species == "virginica",]$Petal.Length

#TO DO: Build a sampling distribution for n = 50 and k = 3
#Note. You can scale the numerator right away (multiply by 50)

#Get the F_value and P-value for those three groups

#Run an ANOVA on Petal.Length ~ Species

#If all went went, the F-statistics and p-values should match
#There is an effect of Species on Petal Length

#Where is the effect?
#TukeyHSD + Confidence intervals will show you the effects


