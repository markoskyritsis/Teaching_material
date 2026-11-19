library(shiny)
library(ggplot2)

# 1. Setup Simulation Data (750 Students)
set.seed(42)
n <- 750
balance <- runif(n, 0, 3000)
# True relationship: Beta0 = -10, Beta1 = 0.005
true_logit <- -10 + 0.005 * balance
prob <- 1 / (1 + exp(-true_logit))
default <- rbinom(n, 1, prob)
df <- data.frame(balance = balance, default = default)

# 2. UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Banking Analytics: Logit Transformation Demo"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Controls"),
      sliderInput("beta1", "Slope (Effect of Balance):", 
                  min = 0.001, max = 0.01, value = 0.005, step = 0.0005),
      hr(),
      h4("Prediction Tool"),
      numericInput("new_bal", "Applicant Balance (£):", 1500, min = 0, max = 5000),
      wellPanel(
        uiOutput("resultsBox")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. The S-Curve (Probability)", plotOutput("probPlot")),
        tabPanel("2. The Logit (Linear)", plotOutput("logitPlot")),
      )
    )
  )
)

# 3. Server
server <- function(input, output) {
  
  # Reactive calculations
  results <- reactive({
    intercept <- -10 # Fixed for simplicity in demo
    logit_val <- intercept + input$beta1 * input$new_bal
    prob_val <- 1 / (1 + exp(-logit_val))
    
    # Odds calculations
    odds_multiplier <- exp(input$beta1 * 100) # Per $100 increase
    pct_change <- (odds_multiplier - 1) * 100
    
    list(prob = prob_val, or = odds_multiplier, pct = pct_change)
  })
  
  output$probPlot <- renderPlot({
    ggplot(df, aes(x = balance, y = default)) +
      geom_point(alpha = 0.2) +
      stat_function(fun = function(x) 1/(1 + exp(-(-10 + input$beta1 * x))), 
                    color = "#002D62", size = 1.5) +
      labs(title = "Probability of Default vs. Balance", y = "P(Default)") +
      theme_minimal()
  })
  
  output$logitPlot <- renderPlot({
    ggplot(df, aes(x = balance)) +
      stat_function(fun = function(x) -10 + input$beta1 * x, 
                    color = "#D4AF37", size = 1.5) +
      labs(title = "Log-Odds vs. Balance (The Link Function)", y = "Logit(P)") +
      theme_minimal()
  })
  
  output$resultsBox <- renderUI({
    res <- results()
    tagList(
      h5("Applicant Prediction:"),
      p(paste0("Probability of Default: ", round(res$prob * 100, 1), "%")),
      tags$b("Business Insight:"),
      p(paste0("Every $100 in balance increases the odds of default by ", 
               round(res$pct, 1), "%."))
    )
  })
}

shinyApp(ui, server)