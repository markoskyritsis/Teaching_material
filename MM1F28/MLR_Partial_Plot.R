library(shiny)
library(ggplot2)
library(car) # For added-variable plots

# 1. Generate a dummy business dataset
set.seed(123)
n <- 750
df <- data.frame(
  budget = runif(n, 100, 1000),
  box_price = runif(n, 10, 50),
  competitor_spend = runif(n, 50, 500),
  region_demand = runif(n, 1, 10)
)
# Create a Y variable with some noise
df$sales <- 50 + (0.5 * df$budget) - (1.2 * df$box_price) + 
  (0.8 * df$region_demand) + rnorm(n, 0, 50)

# 2. Shiny UI
ui <- fluidPage(
  titlePanel("Predictive Analytics: Multiple Linear Regression"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a predictor to see its unique (partial) effect on Sales."),
      selectInput("var", "Visualize Variable:", 
                  choices = c("budget", "box_price", "competitor_spend", "region_demand")),
      hr()),
    #   h4("Manual Prediction"),
    #   numericInput("in_budget", "Budget:", 500),
    #   # ... other inputs ...
    #   actionButton("predict", "Calculate Prediction")
    # ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Partial Regression Plot", plotOutput("avPlot")),
        tabPanel("Model Summary", verbatimTextOutput("summary"))
#        tabPanel("Prediction Result", textOutput("pred_val"))
      )
    )
  )
)

# 3. Shiny Server
server <- function(input, output) {
  # Build the model once
  model <- lm(sales ~ budget + box_price + competitor_spend + region_demand, data = df)
  
  output$summary <- renderPrint({ summary(model) })
  
  output$avPlot <- renderPlot({
    # Use avPlot to show the 'partialled out' relationship
    avPlot(model, variable = input$var, 
           main = paste("Partial Effect of", input$var),
           grid = FALSE, pch = 16, col = "royalblue")
  })
}

shinyApp(ui, server)