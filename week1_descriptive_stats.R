#Libraries I will use for the demonstration
library(wooldridge)
library(tidyverse)

#Online Presentation Week 1 for MM1F28

#####################SUMMARY/DESCRIPTIVE STATISTICS###########
#Note. You will not be required to write code for this module.
#You will be exposed to scripting through this online demonstrations

#To help with the demonstrations, we will use datasets from 
#Jeffrey Wooldridge’s famous econometrics textbook

#Let's start with 'ceosal1' dataset, which contains data on 177 CEOs. 
#It includes their salary, tenure, and company profits.
?ceosal1

#########################CENTRAL TENDANCY###################

#First let's visualise the CEO salaries
# Quick look at the skewness
hist(ceosal1$salary, main="CEO Salaries", xlab="Salary in $1000s", col="lightblue")

#Note the skewness. We'll get back to that.

#############################THE MEAN######################
#The most common average used in statistics. Susceptible to bias, but
#used in further tests (such as t-test, which we will look at later)

#Mean salary of CEOs
mean(ceosal1$salary)

#Note. The mean of 1281.12 is in thousands. This is common
#in business datasets. Make sure to read the documentation/graph
#carefully. The mean salary is $1,291,120

#So that's great, but notice what I said earlier about skewness?
#The mean is susceptible to outliers. Meaning a few very large
#salaries can push the mean up. This does not really reflect
#reality. Here is some proof:
#Temperatures of days in Greece for June (degrees celcius)
Greece_temperatures_june <- c(28,32,35,38,35,32,32,31,697,28,30)
#Average temperature
mean(Greece_temperatures_june)
#~93 degrees celcius!!! How?

#We can see from the dataset that there is an apparatus error:
#It's not possible to have 697 degrees celcius. This unusual
#reading, due to a faulty thermometer, led to an unrepresentative
#statistic. 

#############################THE MEDIAN#########################

#Okay, back to the CEO salaries. We will now use the median instead
median(ceosal1$salary) * 1000
#The median salary is much lower at $1,039,000. One may argue that
#this is still a lot (I don't disagree), but it's more reflective
#of reality for most CEOs

#Just for the sake of completion, here is the median of
#temperature as well:
median(Greece_temperatures_june)
#32 degress celcius, which sounds about right.

#########################The MODE##################
#The mode is the most frequent observation. This type
#of central tendency is great for categorical data.
#I.e., non-numeric.

#Now here is where it gets interesting. Look at the
#dataset once more.
?ceosal1

#Notice indus, finance, consprod, and so on are all
#coded as 1s and 0s.
#We could be tempted to do this:
mean(ceosal1$indus)
#Indeed we get a number: 0.32. But what does that mean?
#The average type of firm is 0.32? It doesn't make
#sense. Not all stats make sense just because you
#crunched some numbers.
#In fact, if we read the documentation we will see that
#indus is CODED as 0 and 1, but is actually YES or NO.
#Either a firm is industrial or it is not. 
#These variables are DUMMY variables (binary indicators).
#For students in both Business and Accounting, understanding 
#how to handle these is vital because so much of your 
#future data (e.g., "Was the audit successful?", 
#"Did the customer churn?") will be coded this way.

# 1. We will wrangle the dummy variables into a single 
#categorical column. This is much more "human-readable"
ceo_industry <- ceosal1 %>%
  mutate(industry_name = case_when(
    indus == 1    ~ "Industrial",
    finance == 1  ~ "Financial",
    consprod == 1 ~ "Consumer Product",
    utility == 1  ~ "Utility/Transport",
    TRUE          ~ "Other"
  ))

# 2. Find the Mode (The most frequent category)
industry_counts <- ceo_industry %>%
  count(industry_name) %>%
  arrange(desc(n))

modal_industry <- industry_counts$industry_name[1]

# 3. Visualize the "Typical" Firm
ggplot(ceo_industry, aes(x = fct_infreq(industry_name), fill = industry_name)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Which Industry Employs the Most CEOs?",
    subtitle = paste("The Modal Industry is:", modal_industry),
    x = "Industry Type",
    y = "Number of CEOs",
    fill = "Industry"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Legend is redundant here

#We can see, from the graph, that the modal industry is
#in fact, industrial

#Note. We can have NO mode and we can have more than one
#mode.
#For example, in the following set:
a <- c("red","red","blue","green","yellow")
#The mode is obviously red

#However, in the following set:
b <- c("red","red","blue","green","blue")
#The mode is red AND blue. This is called bimodal

#Finally, in the following set:
b <- c("red","yellow","blue","green","white")
#There is no mode


#######################Measures of Spread###########

#In both finance and accounting, spread is the 
#foundation of Risk
#Let's take a look at these two sets. Which one do
#you think is more spread out? set a, or set b?
a <- c(50,51,53,49,48,47,50)
b <- c(10,71,23,119,48,47,500)

#Intuitively, you may think: definitely b. The numbers
#are further away from each other. You would be correct.

#The first thing we can ask is, how far away are they?

#################Variance################

#Variance is a measure of spread. It quantifies the
#spread in our data giving us a number.
#You can find the formula in the lecture slide, but 
#you don't need to memorise it. Let's use R instead:

var(a)
var(b)

#The variance for b is much larger. 
#Back to the ceosalaries

var(ceosal1$salary)*1000


#The variance is $1.88x10^9 squared dollars.

#Wait! What are squared dollars?
#This is not human-readable. 
#Furthermore the variance is NOT standardised.

#################Standard Deviation###############
#Instead of the variance, which is used for further
#statistical tests. For human readability we can take
#the standard deviation instead.
sd(ceosal1$salary)*1000
#The standard deviation is $1,372,345

#But what does that actually mean?

###################Empirical Rule (68-95-99.7)
#This is a good time to introduce the normal distribution
#and how the empirical rule ties in to the standard
#deviation

library(tidyverse)

# 1. Generate a Theoretical Normal Distribution
# We'll use a mean of 0 and SD of 1 (Standard Normal)
x <- seq(-4, 4, length.out = 1000)
y <- dnorm(x, mean = 0, sd = 1)
df <- data.frame(x, y)

# 2. Plot with Shading for the Empirical Rule
ggplot(df, aes(x = x, y = y)) +
  geom_line(size = 1) +
  # Shading 1 SD (68.2%)
  geom_area(data = filter(df, x >= -1 & x <= 1), fill = "#3498db", alpha = 0.5) +
  # Shading 2 SD (95.4%)
  geom_area(data = filter(df, (x >= -2 & x <= -1) | (x >= 1 & x <= 2)), fill = "#3498db", alpha = 0.3) +
  # Shading 3 SD (99.7%)
  geom_area(data = filter(df, (x >= -3 & x <= -2) | (x >= 2 & x <= 3)), fill = "#3498db", alpha = 0.1) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "The Empirical Rule (The 68-95-99.7 Rule)",
    subtitle = "Shaded areas represent ±1, ±2, and ±3 Standard Deviations",
    x = "Z-score (Standard Deviations from Mean)",
    y = "Density"
  ) +
  annotate("text", x = 0, y = 0.2, label = "68%", color = "white", fontface = "bold") +
  theme_minimal()

#Under the normal distribution (the bell curve), 68%
#of all observations will be within one standard deviation
#from the mean. 95% will be within two standard deviations
#99.7% will be within three.

#HOWEVER, the issue here is that ceosalaries do NOT
#follow the normal distribution. If they did we would
#say that 68% of all CEOs have salaries ranging from
mean(ceosal1$salary) - sd(ceosal1$salary)

#In the Real World of Business, most variables 
#(salaries, stock prices, insurance claims) 
#have a lower bound of zero. You can't have a 
#negative salary (unless you're paying the company 
#to work there, which even in this economy is rare!).

#This is why we often use the LOGARITHM:
hist(ceosal1$lsalary, main="CEO Log Salaries", xlab="Log of Salary in $1000s", col="lightblue")

mean(ceosal1$lsalary) - sd(ceosal1$lsalary)

#By moving to the log-scale, we’re effectively 
#shifting the conversation from absolute currency 
#to proportional change. In the log-world, the 
#"negative" result of a subtraction doesn't 
#represent a negative salary; it simply 
#represents a very small positive salary 
#when exponentiated back.


####################The trap of summary statistics

#Even today, in consulting documents we see summary
#statistics being used erroneously. To wrap up for today
#I will show you why you should never rely on just
#summary statistics in order to infer findings from
#a sample to a population

# 1. The "Truth" (Population Mean.)
#Note. For the sake of argument, let's say that our population
#is these 209 CEOs. We define a population after all.

#We will use Monte Carlo Simulations to see what happens when
#we take small samples and look at their averages

true_avg <- mean(ceosal1$salary)

# 2. A classic mistake (Single small sample)
set.seed(123) # For reproducibility
my_sample <- sample(ceosal1$salary, 5)
sample_avg <- mean(my_sample)
print(sample_avg*1000)

# 3. Let's do this 10,000 times and see the results
many_samples <- replicate(10000, mean(sample(ceosal1$salary, 5)))

ggplot(data.frame(x = many_samples), aes(x = x)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  geom_vline(xintercept = true_avg, color = "red", size = 1.5) +
  annotate("text", x = true_avg + 300, y = 500, label = "The Real Average", color = "red") +
  labs(title = "Distribution of 10,000 Sample Means (n=5)",
       subtitle = "Notice how often a small sample 'lies' about the truth!",
       x = "Calculated Sample Mean Salary",
       y = "Frequency") +
  theme_minimal()


####The law of large numbers
#Some of you will be asking yourselves, what if you had a larger
#sample size. I.e., not just five people? Well, let's take a look.


# 1. Define the parameter (population mean)
population_mean <- mean(ceosal1$salary * 1000)

# 2. Set up the Monte Carlo Parameters
sample_sizes <- c(5, 10, 15, 20, 25)
iterations <- 1000

# 3. Run the Simulation
results <- map_df(sample_sizes, function(n) {
  # For each size n, repeat 1000 times: 
  # Calculate distance |Sample Mean - Population Mean|
  errors <- replicate(iterations, {
    s <- sample(ceosal1$salary * 1000, n)
    abs(mean(s) - population_mean)
  })
  
  data.frame(n = as.factor(n), error = errors)
})

# 4. Visualize the "Convergence"
ggplot(results, aes(x = n, y = error, fill = n)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "The Cost of Small Samples",
    subtitle = "Distance between Sample Mean and True Population Mean ($1.28M)",
    x = "Sample Size (n)",
    y = "Absolute Error (Distance from Truth)",
    fill = "Sample Size"
  ) +
  theme_minimal()

#####Conclusion:
##1. The "Shrinking" Boxes 
#The box and whisker plots (which represent the middle 
#50% of our guesses) get shorter and closer to the bottom 
#(zero error) as we move from left to right. This is precision 
#increasing.

#2.#The Outliers (The "Black Swans"): 
#At n=5, you’ll see dots way up the y-axis. 
#"These are the studies that only looked at 5 CEOs and 
#accidentally concluded the average salary was $2M higher 
#than it actually is.

#3. "The Business Argument

#Audit Risk. "If you only check 5 invoices (n=5), 
#your estimate of total company spending could be off by 
#hundreds of thousands. If you check 25, your risk of a 
#massive error drops significantly."

write.csv(ceosal1,"ceo_salaries.csv")
