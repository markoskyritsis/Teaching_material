library(shiny)
library(ggplot2)
library(shinythemes)

# 1. DATA SIMULATION
set.seed(750)
n <- 750
balance <- runif(n, 0, 4000)
# Real relationship: -8 Intercept, 0.004 Slope
true_logit <- -8 + 0.004 * balance
prob <- 1 / (1 + exp(-true_logit))
default <- rbinom(n, 1, prob)
banking_data <- data.frame(balance = balance, default = default)

# 2. UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Banking Analytics: Interpreting GLM Output"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Simulation"),
      p("Analyze how monthly balance predicts credit default for 750 customers."),
      sliderInput("user_bal", "Predict for Balance (£):", 0, 5000, 2000),
      hr(),
      tags$b("The Formula:"),
      withMathJax(p("$$\\text{logit}(p) = \\beta_0 + \\beta_1(\\text{Balance})$$"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. R Output (Raw)", 
                 verbatimTextOutput("rawSummary"),
                 wellPanel(
                   h4("Pseudo R-Squared (McFadden)"),
                   textOutput("pseudoR2")
                 )),
        tabPanel("2. Business Interpretation", 
                 uiOutput("interpretBox")),
        tabPanel("3. Visual Fit", 
                 plotOutput("fitPlot"))
      )
    )
  )
)

# 3. SERVER
server <- function(input, output) {
  
  # Fit the model
  model <- glm(default ~ balance, data = banking_data, family = "binomial")
  
  output$rawSummary <- renderPrint({
    summary(model)
  })
  
  output$pseudoR2 <- renderText({
    null_dev <- model$null.deviance
    res_dev <- model$deviance
    r2 <- 1 - (res_dev / null_dev)
    paste0(round(r2 * 100, 2), "% of the 'misfit' (deviance) was reduced by this model.")
  })
  
  output$interpretBox <- renderUI({
    b0 <- coef(model)[1]
    b1 <- coef(model)[2]
    or <- exp(b1)
    pct <- (or - 1) * 100
    
    tagList(
      h3("Deciphering the Coefficients"),
      tags$ul(
        tags$li(tags$b("The Intercept: "), round(b0, 4), 
                " (The log-odds of default if balance is $0)"),
        tags$li(tags$b("The Slope (Log-Odds Ratio): "), round(b1, 4), 
                " (For every $1 increase, log-odds go up by this amount)"),
        tags$li(tags$b("The Odds Ratio: "), round(or, 4), 
                " (For every $1 increase, the odds of default are multiplied by this factor)")
      ),
      hr(),
      wellPanel(
        style = "background-color: #fcf8e3; border-color: #faebcc;",
        h4("The Boardroom Sentence:"),
        p(paste0("Our model shows that for every $1 increase in a customer's monthly balance, the "),
          tags$b("odds of default increase by ", round(pct, 2), "%.")),
        tags$small("Note: We use percentage change in ODDS because the change in PROBABILITY is not constant.")
      )
    )
  })
  
  output$fitPlot <- renderPlot({
    ggplot(banking_data, aes(x = balance, y = default)) +
      geom_jitter(height = 0.05, alpha = 0.2, color = "darkblue") +
      stat_smooth(method = "glm", method.args = list(family = "binomial"), 
                  color = "#D4AF37", size = 1.5) +
      labs(title = "The S-Curve Fit for Banking Data",
           x = "Monthly Balance ($)", y = "Probability of Default") +
      theme_minimal()
  })
}

shinyApp(ui, server)