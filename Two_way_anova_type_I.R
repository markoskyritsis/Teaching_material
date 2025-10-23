#Error rate inflation when running two one-way ANOVAs?

#Define a population with some parameters
pop <- rnorm(100000,100,30)

#We will put this in a loop in order to test our type I
error_rate <- 0

for (i in 1:10000) {

  #Take a sample from the population
  samp <- sample(pop, 90)
  
  #Okay, let's say our first IV is 'education', note that I'm just assigning
  #the first thirty people, then the next thirty, and so on. There is no
  #effect here.
  highschool <- samp[1:30]
  bachelors <- samp[31:60]
  masters <- samp[61:90]
  
  #Let's start simple. ANOVA should not show an effect here, but we expect a
  #5% type 1 error rate.
  
  Data <- c(highschool, bachelors, masters)
  Group <- factor(c(rep("highschool", 30), rep("bachelors", 30), rep("masters", 30)))
  ANOVA_Data <- data.frame(Data, Group)
  
  p_value <- summary(aov(ANOVA_Data$Data ~ ANOVA_Data$Group))[[1]][["Pr(>F)"]][1]

  if (p_value < 0.05) {
    error_rate = error_rate + 1
  }  
}
#Should be around 5%
print(error_rate/10000)

#Okay, now that we've established this, let's copy-paste the simulation above
#This time will have two IVs

#We will put this in a loop in order to test our type I
error_rate <- 0

for (i in 1:10000) {
  
  samp <- sample(pop, 90)
  
  highschool <- samp[1:30]
  bachelors <- samp[31:60]
  masters <- samp[61:90]
  
  Data <- c(highschool, bachelors, masters)
  Group <- factor(c(rep("highschool", 30), rep("bachelors", 30), rep("masters", 30)))
  ANOVA_Data <- data.frame(Data, Group)
  
  p_value <- summary(aov(ANOVA_Data$Data ~ ANOVA_Data$Group))[[1]][["Pr(>F)"]][1]
  
  if (p_value < 0.05) {
    error_rate = error_rate + 1
  }  
  
  #Now for the second IV. Let's say we also want to see if there is an
  #effect of sex on the DV
  #Our expression is now: DV ~ education + sex

  Group2 <- factor(c(rep("male", 45), rep("female", 45)))
  ANOVA_Data <- data.frame(Data, Group, Group2)
  
  p_value <- summary(aov(ANOVA_Data$Data ~ ANOVA_Data$Group2))[[1]][["Pr(>F)"]][1]
  
  if (p_value < 0.05) {
    error_rate = error_rate + 1
  }  
  
}

#Let's check the total error rate:
print(error_rate/10000)
#Should be around 10% (as we'd expect). 

#Now we will use two-way ANOVA

error_rate <- 0

for (i in 1:10000) {
  
  samp <- sample(pop, 90)

  #IV 1  
  highschool <- samp[1:30]
  bachelors <- samp[31:60]
  masters <- samp[61:90]
  
  Data <- c(highschool, bachelors, masters)
  Group <- factor(c(rep("highschool", 30), rep("bachelors", 30), rep("masters", 30)))

  #IV 2 
  Group2 <- factor(c(rep("male", 45), rep("female", 45)))
  ANOVA_Data <- data.frame(Data, Group, Group2)
  
  p_value <- summary(aov(ANOVA_Data$Data ~ ANOVA_Data$Group + ANOVA_Data$Group2))[[1]][["Pr(>F)"]][1]
  
  if (p_value < 0.05) {
    error_rate = error_rate + 1
  }  
  
}

#Let's check the total error rate:
print(error_rate/10000)

#Should be around 5% again. Therefore, just like ANOVA controls type I
#error rate when we have > 2 comparisons. Two-way ANOVA controls type I
#when we have > 1 IV.


