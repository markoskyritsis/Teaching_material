library(shiny)
library(ggplot2)
library(shinythemes)

# 1. DATA SIMULATION
set.seed(101)
n <- 750
balance <- runif(n, 500, 3500)
student <- sample(c("Non-Student", "Student"), n, replace = TRUE)

# Simulation Logic: 
# Being a student reduces the log-odds by 1.5 (The "Protective" effect)
# Intercept -9, Balance coefficient 0.004
logit_val <- -9 + 0.004 * balance + ifelse(student == "Student", -1.5, 0)
prob <- 1 / (1 + exp(-logit_val))
default <- rbinom(n, 1, prob)
df_cat <- data.frame(balance = balance, student = student, default = default)

# 2. UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Categorical Predictors: The 'Student' Effect"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Group Comparison"),
      p("We are comparing how Student Status shifts the risk of default."),
      hr(),
      helpText("In R, 'Non-Student' is the baseline (0) and 'Student' is the dummy variable (1)."),
      hr(),
      wellPanel(
        h4("The Logic"),
        p("Observe how the curves are parallel in the 'Log-Odds' view, but shifted in the 'Probability' view.")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Probability (S-Curves)", 
                 plotOutput("probPlotCat"),
                 wellPanel(
                   h4("Interpretation"),
                   p("At any given balance, the Student curve (Yellow) is lower than the Non-Student curve (Blue).")
                 )),
        tabPanel("Log-Odds (Parallel Lines)", 
                 plotOutput("logitPlotCat"),
                 wellPanel(
                   h4("The Math"),
                   p("The distance between these lines is the coefficient for 'Student'. They stay perfectly parallel.")
                 ))
      )
    )
  )
)

# 3. SERVER
server <- function(input, output) {
  
  # 1. Probability Plot
  output$probPlotCat <- renderPlot({
    ggplot(df_cat, aes(x = balance, y = default, color = student)) +
      geom_jitter(height = 0.03, alpha = 0.3) +
      stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, size = 1.5) +
      scale_color_manual(values = c("Non-Student" = "#2c3e50", "Student" = "#f39c12")) +
      labs(title = "P(Default) Shifted by Student Status",
           x = "Monthly Balance ($)", y = "Probability", color = "Group") +
      theme_minimal()
  })
  
  # 2. Log-Odds Plot
  output$logitPlotCat <- renderPlot({
    # We manually calculate the logit lines for visualization
    # Using a simple fit for the demo
    fit <- glm(default ~ balance + student, data = df_cat, family = "binomial")
    b0 <- coef(fit)[1]
    b_bal <- coef(fit)[2]
    b_stu <- coef(fit)[3]
    
    ggplot(df_cat, aes(x = balance, color = student)) +
      geom_abline(intercept = b0, slope = b_bal, color = "#2c3e50", size = 1.2) + # Non-Student
      geom_abline(intercept = b0 + b_stu, slope = b_bal, color = "#f39c12", size = 1.2) + # Student
      xlim(500, 3500) + ylim(-10, 5) +
      labs(title = "Log-Odds: The Parallel Assumption",
           subtitle = "The 'Student' coefficient just changes the intercept",
           x = "Monthly Balance (£)", y = "Log-Odds", color = "Group") +
      theme_minimal()
  })
}

shinyApp(ui, server)