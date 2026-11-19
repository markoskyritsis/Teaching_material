library(shiny)
library(ggplot2)

# 1. Setup Synthetic Data
set.seed(123)
n <- 750
balance <- runif(n, 0, 5000)
# Create a probability centered around a $2500 balance
prob_default <- 1 / (1 + exp(-0.002 * (balance - 2500)))
default_status <- rbinom(n, 1, prob_default)
df <- data.frame(balance = balance, default = default_status)

# 2. UI Definition
ui <- fluidPage(
  titlePanel("The Failure of Linear Regression for Probabilities"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This app shows why Simple Linear Regression (SLR) is 
               unsuitable for binary outcomes like 'Default'."),
      sliderInput("intercept", "Adjust Intercept:", -0.5, 0.5, 0),
      sliderInput("slope", "Adjust Slope:", 0.0001, 0.0005, 0.0002, step = 0.00005),
      hr(),
      tags$b("The Problem:"),
      tags$p("Notice where the blue line exits the 0 to 1 range.")
    ),
    
    mainPanel(
      plotOutput("regressionPlot"),
      wellPanel(
        uiOutput("warningText")
      )
    )
  )
)

# 3. Server Logic
server <- function(input, output) {
  
  output$regressionPlot <- renderPlot({
    ggplot(df, aes(x = balance, y = default)) +
      geom_point(alpha = 0.3, color = "darkblue") +
      # Add the SLR Line
      geom_abline(intercept = input$intercept, 
                  slope = input$slope, 
                  color = "red", size = 1.2) +
      # Bounds indicators
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_y_continuous(breaks = seq(-0.5, 1.5, 0.25)) +
      labs(title = "Predicting p(Default) using Monthly Balance",
           subtitle = "Red Line = Simple Linear Regression",
           x = "Monthly Balance (£)",
           y = "Probability of Default") +
      theme_minimal()
  })
  
  output$warningText <- renderUI({
    # Logic to check for "out of bounds" predictions
    max_pred <- input$intercept + (input$slope * 5000)
    min_pred <- input$intercept + (input$slope * 0)
    
    if(max_pred > 1 || min_pred < 0) {
      helpText(style = "color: red; font-weight: bold;",
               paste("CRITICAL ERROR: Predictions range from", 
                     round(min_pred, 2), "to", round(max_pred, 2), 
                     ". You cannot have a probability outside [0, 1]!"))
    } else {
      helpText("Adjust the sliders to see how SLR behaves.")
    }
  })
}

shinyApp(ui = ui, server = server)
