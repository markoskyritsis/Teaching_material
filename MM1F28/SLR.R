library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Intro to Predictive Analytics: Marketing vs. Sales"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Adjust the strength of the relationship (Correlation)."),
      sliderInput("corr", "Correlation Strength (r):", 
                  min = 0, max = 1, value = 0.7, step = 0.05),
      hr(),
      tags$div(
        tags$h4("Regression Equation:"),
        uiOutput("formula")
      )
    ),
    
    mainPanel(
      plotOutput("regressionPlot"),
      wellPanel(
        helpText("Notice how the line flattens and coefficients shift as noise increases.")
      )
    )
  )
)

server <- function(input, output) {
  
  # Generate synthetic data based on the correlation slider
  data_react <- reactive({
    set.seed(123) # Keep points consistent while sliding
    n <- 100
    marketing <- runif(n, 50, 500) # Marketing budget between 50 and 500
    
    # Logic to control correlation: 
    # We mix the target variable with noise based on the input 'r'
    noise <- rnorm(n, mean = 0, sd = 100)
    r <- input$corr
    
    # Scale sales to roughly match marketing budget magnitude
    sales <- (r * marketing) + ((1 - r) * noise * 2) + 100
    
    data.frame(x = marketing, y = sales)
  })
  
  # Calculate the linear model
  model_react <- reactive({
    lm(y ~ x, data = data_react())
  })
  
  output$regressionPlot <- renderPlot({
    df <- data_react()
    fit <- model_react()
    
    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = "steelblue", alpha = 0.6, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "darkorange", size = 1.5) +
      labs(x = "Marketing Budget ($)", y = "Total Sales ($)") +
      theme_minimal()
  })
  
  
  output$formula <- renderUI({
    fit <- model_react()
    b0 <- round(coef(fit)[1], 2)
    b1 <- round(coef(fit)[2], 2)
    s <- summary(model_react())
    # Displaying the mathematical notation
    withMathJax(
      paste0("$$\\hat{y} = ", b0, " + ", b1, "x_1$$","R-squared:", round(s$r.squared, 3))
    )
    
  })
}

shinyApp(ui = ui, server = server)