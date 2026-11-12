library(shiny)
library(ggplot2)

# --- Define the User Interface (UI) ---
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"), # Using a clean theme
  titlePanel("Case Study: Retail Store Foot Traffic & Sales"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("1. Define Store Characteristics ('The Truth')"),
        # The true "Average baseline sales" (beta_0)
        sliderInput("true_intercept", "Baseline Sales ($):", 
                    min = 0, max = 500, value = 150, step = 10),
        
        # The true "Average value per customer" (beta_1)
        sliderInput("true_slope", "Avg. Sales Per Customer ($):", 
                    min = 5, max = 50, value = 22, step = 1),
        
        # The true noise (Standard deviation of epsilon)
        sliderInput("true_noise", "Random Daily Noise (High = More Unpredictable):", 
                    min = 5, max = 150, value = 60, step = 5)
      ),
      hr(),
      wellPanel(
        h4("2. Statistical Results ('Our Estimates')"),
        htmlOutput("est_formula"),
        strong("R-Squared:"), textOutput("est_rsquared")
      )
    ),
    
    mainPanel(
      fluidRow(
        column(8,
               h4("Predicted Revenue vs. Reality"),
               plotOutput("mainRegressionPlot")
        ),
        column(4,
               wellPanel(
                 h5("Interpretation"),
                 p("The red line is our prediction (y-hat)."),
                 p("The shaded gray area is the ", 
                   strong("95% Confidence Interval,"), 
                   "showing us where the 'true' relationship line likely hides.")
               )
        )
      ),
      hr(),
      fluidRow(
        h4("Diagnostics: Residual Plot (Our measurable errors, 'e')"),
        plotOutput("residualPlot", height = "250px"),
        wellPanel(
          p("Each dot is a day's revenue minus our prediction. A patternless, random scatter means our model is working well.")
        )
      )
    )
  )
)

# --- Define the Server Logic ---
server <- function(input, output) {
  
  # 1. Generate Synthetic Data based on user-defined parameters ('The Truth')
  simulated_data <- reactive({
    set.seed(42) # Consistent random generation
    n_days <- 90 # Simulate 3 months of data
    
    # Independent Variable: Customer Traffic (roughly normal)
    traffic <- round(rnorm(n_days, mean = 200, sd = 40))
    traffic[traffic < 0] <- 0 # Can't have negative customers
    
    # Simulate the true underlying process (y = beta0 + beta1*x + epsilon)
    # y = Baseline + (Avg Spend * Traffic) + Random Noise
    revenue <- input$true_intercept + (input$true_slope * traffic) + rnorm(n_days, mean = 0, sd = input$true_noise)
    revenue[revenue < 0] <- 0 # Can't have negative revenue
    
    data.frame(Traffic = traffic, Revenue = revenue)
  })
  
  # 2. Run the actual Linear Model (estimating the parameters)
  fit_model <- reactive({
    lm(Revenue ~ Traffic, data = simulated_data())
  })
  
  # --- Visualization Outputs ---
  
  # Output A: The Main Regression Plot with Confidence Interval
  output$mainRegressionPlot <- renderPlot({
    df <- simulated_data()
    
    ggplot(df, aes(x = Traffic, y = Revenue)) +
      geom_point(alpha = 0.6, color = "dodgerblue3", size = 2.5) +
      # 'se = TRUE' automatically adds the 95% Confidence Interval
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "darkred", size = 1.2, fill = "gray80") +
      labs(x = "Daily Customer Traffic", y = "Total Daily Revenue ($)") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # Output B: The Residual Plot
  output$residualPlot <- renderPlot({
    df <- simulated_data()
    fit <- fit_model()
    # Add residuals (errors) and fitted (predictions) values to the dataframe
    df$residuals <- residuals(fit)
    df$fitted <- fitted(fit)
    
    ggplot(df, aes(x = fitted, y = residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", size = 1) +
      geom_point(color = "purple3", alpha = 0.7, size = 2) +
      # Adding subtle vertical lines to emphasize distance from 0
      geom_segment(aes(xend = fitted, yend = 0), color = "gray90", alpha = 0.5) +
      labs(x = "Predicted Revenue ($) (y-hat)", y = "Actual Residual Error ($) (e)") +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # --- Numerical Outputs (Summaries) ---
  
  output$est_formula <- renderUI({
    fit <- fit_model()
    b0 <- round(coef(fit)[1], 2)
    b1 <- round(coef(fit)[2], 2)
    
    # Render using MathJax (or a clean HTML substitute for simplicity)
    HTML(paste0("Our Model Estimates:<br><strong>&ycirc; = ", b0, " + ", b1, "(Traffic)</strong>"))
  })
  
  output$est_rsquared <- renderText({
    summary(fit_model())$r.squared |> round(3)
  })
}

# --- Run the Shiny App ---
shinyApp(ui = ui, server = server)