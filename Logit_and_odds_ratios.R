#Logit models are simple but powerful. They may not have the predictive
#capabilities of more contemporary MLs, but they are a first step
#to this world, and they give use odds rations and probabilities
#of an event occuring. Let's start by picking a suitable dataset
library(car)

df <- TitanicSurvival

#check the documentation
?TitanicSurvival

#NOTE. There is another titanic survival dataset called "Titanic". I opted
#to use TitanicSurvival because one of the predictors is numeric (age). This
#is more for learning purposes

#Why titanic? Why not, say, a more contemporary business-related dataset?
#Simply put, modern business datasets are quite busy, and often very messy
#I want you to understand the models first, before we start using them
#on more business-oriented problems. It's hard to understand these
#concepts if you are fighting against messy data.



##Step 1: Odds ratios and probabilities

#The response variable here is "survived". Either a passanger made it out
#alive or they didn't. 

#Let's start by looking at the odds of a passanger surviving given their sex

fit <- glm(survived ~ sex, data = df, family = "binomial")

summary(fit)

#First thing to note right away is that only males are shown. The female
#passengers are the intercept (what we are comparing against). 

#Second thing to note is that the coefficient for sexmale is negative.
#That immediately tells me that they were LESS likely to survive the
#titanic. But by how much?


#We take the exponent of the second coefficient. Here is a way to do it
#with code if you don't want to simply copy-paste -2.425438. It's easy to
#do for small models (the first coefficient is always the intercept), so this
#is the second coefficient
exp(fit$coefficients[2])

#You'll note that the result is much smaller than one. The odds ratios are
#as follows:

##The odds of survival for a male passenger are 0.088 times the odds of
##survival for a female passenger.

#Sometimes this doesn't make intuitive sense, so we take percentages instead.
#Since the odds are < 1, we simply subtract from 1 and multiply by 100

(1 - exp(fit$coefficients[2])) * 100

#Male passengers were 91.16% less likely to make it out alive than female
#passengers. 

#Okay, let's look at a numeric variable next. We'll use age for this one

fit <- glm(survived ~ age, data = df, family = "binomial")

summary(fit)

#You'll note that age is NOT a significant predictor. This doesn't make
#much sense. 


#We'll get back to that in a minute, but let's interpret the results real
#quick

exp(fit$coefficients[2])

#With each unit of increase in age the odds of survival decrease by
# a factor of 0.99

#This is less than 1 so:

(1 - exp(fit$coefficients[2])) * 100

#With each unit of increase in age, the chances of survival decrease by
#just under 1%


#Why does this result not reflect the full truth?
#On the titanic oldest passengers tended to be in first class. 
#Here is proof:

fit_lm <- lm(df$age ~ df$passengerClass)
summary(fit_lm)

#2nd class on average 9.6 years younger
#3rd class 14.34 years younger

##The first model I showed you was UNCONTROLLED. In other words, we did
#not take into account any control variables. The women and children policy was applied mostly to
#first class passengers. Social status had an impact. But I digress, let's
#add passenger class as a predictor

fit <- glm(survived ~ age + passengerClass, data = df, family = "binomial")
summary(fit)

#The model has assigned a large statistically significant coefficient
#to passenger class. By controlling the influence of passenger class,
#we now isolate the effect of age on survival at a fixed passenger class
#In other words, for people in the same passenger class, older passengers
#were significantly less likely to survive. But by how much?

exp(fit$coefficients[2])

#With each unit increase in age, the odds decrease by a factor of 0.96

(1 - exp(fit$coefficients[2])) * 100

#With each unit increase in age, the chances of survival drop by 3.67%

##Was this because of good policy? Probably not. Mobility played a more
#significant role (whoever gets there first)




##Advanced modelling concepts.
#So here is the reality of what actually happened in the titanic.
#First we need to really think about what the most appropriate model 
#would be. My hypothesis Model1 is that passenger class was a MODERATOR
#for the policy. It was not applied uniformly across classes.

#In other words, the relationship between sex and survival depends on your
#class. The same for age.


Model1 <- glm(survived ~ (age +  sex) * passengerClass, data = df, family = "binomial")
summary(Model1)

#As you can see the results now tell a very, very different story. 
#First class is the intercept (the reference level)

#Being a male in first class, DECREASES odds of survival:
exp(-3.853478) #odds of survival are 0.02 of female survival

#The lack of statistical significance in second class tells us that
#the policy favouring women and penalizing men was applied in second class

#Being a male in third class, INCREASES odds of survival
exp(2.438650) #odds of survival are 11.46 of female survival

#Women first policy applied mostly to first and second class. Women of third class
#did not have a good survival rate.
#How many of the passengers were women of the third class?
(nrow(df[df$sex == "female" & df$passengerClass == "3rd",]) / nrow(df)) * 100

#~17%




##Age (children first)
#First class
exp(-0.031094) #each unit increase in age, log-odds decrease by 0.031


#Second class
#Total effect is actually -0.031094 + -0.052393 = -0.083
exp(-0.083) 
#As you can see, in the second class the penalty for being older was
#higher

#Good policy, or simply mobility? 
#There is a lack of effect of age in third class. This is telling, what
#mattered most is getting out (not to mention that the age is skewed for
#that class, since average age is 14years younger)



#Obviously, the most striking effect is that passengers in third class
#were much less likely to make it out alive:
exp(-4.013866)
#odds-ratio on 0.02, or if you prefer
(1 - exp(-4.013866)) * 100
#98% less likely to make it out alive...


##I hope this helped you understand odds-ratios, logit models, and of course
#how the models can help us understand some core business-related concepts
#such as whether policy was applied correctly, etc.


##########################Model comparisons
#It's interesting to see how much more likely a model is over some other
#model to be the true model

#So you don't have to scroll up
Model1 <- glm(survived ~ (age +  sex) * passengerClass, data = df, family = "binomial")

#Here are a few other models
#Model 2 is just age
Model2 <- glm(survived ~ age, data = df, family = "binomial")
#Model 3 is just sex
Model3 <- glm(survived ~ sex, data = df, family = "binomial")
#Model 4 is just passenger class
Model4 <- glm(survived ~ passengerClass, data = df, family = "binomial")
#Model 5 is a good comparison since it is a simple but full model
Model5 <- glm(survived ~ age +  sex + passengerClass, data = df, family = "binomial")
#Let's compare these models to my hypothesis

#Wagenmaker formula
#BFworse_better <- exp((BIC(worse_model) - BIC(better_Model))/2)

bf21 <- exp((BIC(Model2) - BIC(Model1))/2)
print(bf21)

#Model 1 is 4.59 x 10^95 times more likely to fit the data than model 2

#Let's be realistic and look at just passenger class, and then the full model
bf41 <- exp((BIC(Model4) - BIC(Model1))/2)
print(bf41)
#1.42788e+141 times

bf51 <- exp((BIC(Model5) - BIC(Model1))/2)
print(bf51)
#11229753 time more likely (11 million times more likely)

# Bayes Factor (BF51),Strength of Evidence for M1
# 1 to 3,Barely worth mentioning
# 3 to 10,Substantial
# 10 to 30,Strong
# 30 to 100,Very Strong
# >100,Decisive

#We can conclude that this is decisive evidence in support of our
#complex model over the full model at capturing the data




