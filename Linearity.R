### The importance of visualising the data first.
## Let's generate some non-linear relationships

### Quadratic

# --- 1. QUADRATIC DATASET (U-Shape / Parabola) ---
set.seed(42) # For reproducibility

# Independent Variable (X): 
X_quad <- seq(-10, 10, length.out = 50) 

# Dependent Variable (Y): Y = 2 * X^2 + small error
Y_quad <- 2 * X_quad^2 + rnorm(50, mean = 0, sd = 10) 

quadratic_data <- data.frame(Time_X = X_quad, Performance_Y = Y_quad)

# Visualization in R:
 plot(quadratic_data$Time_X, quadratic_data$Performance_Y, main="Quadratic Relationship")
 abline(lm(Performance_Y ~ Time_X, data = quadratic_data), col="red")
 
 #P-value suggests no relationship
 summary(lm(Performance_Y ~ Time_X, data = quadratic_data))
 
 
 
 # --- 2. EXPONENTIAL DATASET (Viral Growth) ---
 set.seed(42)
 
 # Independent Variable (X): Time (Weeks)
 X_exp <- 1:50 
 
 # Dependent Variable (Y): User Adoption/Viral Reach + some noise
 Y_exp <- 10 * exp(0.2 * X_exp) + rnorm(50, mean = 0, sd = 100) 
 
 exponential_data <- data.frame(Time_X = X_exp, User_Reach_Y = Y_exp)
 
 # Visualization in R:
  plot(exponential_data$Time_X, exponential_data$User_Reach_Y, main="Exponential Relationship")
  abline(lm(User_Reach_Y ~ Time_X, data = exponential_data), col="red")
  
  #P-value suggests moderate relationship (not true)
  summary(lm(User_Reach_Y ~ Time_X, data = exponential_data))


  # --- 3. SINE WAVE DATASET (Seasonal Sales) ---
  set.seed(42)
  
  # Independent Variable (X): Days of the Year
  X_sin <- 1:100 
  
  # Dependent Variable (Y): Sales Volume (Cyclical)
  # Y = 50 * sin(X/10) + 100 (shifts the wave up) + error
  Y_sin <- 50 * sin(X_sin / 10) + 100 + rnorm(100, mean = 0, sd = 5) 
  
  sine_data <- data.frame(Day_X = X_sin, Sales_Volume_Y = Y_sin)
  
  # Visualization in R:
   plot(sine_data$Day_X, sine_data$Sales_Volume_Y, main="Cyclical/Periodic Relationship")
   abline(lm(Sales_Volume_Y ~ Day_X, data = sine_data), col="red")
  
   #P-value suggests no relationship (not true)
   summary(lm(Sales_Volume_Y ~ Day_X, data = sine_data))
   
   
  ###Advanced concept polynomial regression
   #For the first one we had
   summary(lm(Performance_Y ~ Time_X, data = quadratic_data))
   
   #When we should have had
   summary(lm(Performance_Y ~ Time_X + I(Time_X^2), data = quadratic_data))
   