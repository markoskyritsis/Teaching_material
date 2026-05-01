library(shiny)
library(ggplot2)

ui <- fluidPage(
  theme = shinythemes::shinytheme("cyborg"), # A dark theme for a "Casino" feel
  titlePanel("The R-Squared Casino: Can Noise 'Improve' a Model?"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Step 1: The Real Model"),
      p("We have 100 customers. We know their 'Budget' predicts their 'Sales'."),
      hr(),
      h4("Step 2: Add the Noise"),
      sliderInput("num_noise", "Number of Random Noise Variables to add:", 
                  min = 0, max = 20, value = 0, step = 1),
      helpText("These variables are just random numbers. They have zero real connection to Sales."),
      hr(),
      h4("The Results"),
      tableOutput("r2_table")
    ),
    
    mainPanel(
      plotOutput("r2_comparison"),
      br(),
      p("Notice: As you add more 'Noise', the Multiple R-Squared always goes up or stays the same. The Adjusted R-Squared is less prone to overparameterisation.")
    )
  )
)

server <- function(input, output) {
  
  # Create a base dataset
  set.seed(42)
  n <- 100
  base_df <- data.frame(
    budget = runif(n, 100, 1000),
    sales = 50 + (0.5 * runif(n, 100, 1000)) + rnorm(n, 0, 50)
  )
  
  # Add 20 columns of pure noise
  noise_matrix <- matrix(rnorm(n * 20), ncol = 20)
  colnames(noise_matrix) <- paste0("noise_", 1:20)
  full_df <- cbind(base_df, noise_matrix)
  
  # Reactive Calculations
  model_results <- reactive({
    # Build formula: sales ~ budget + noise_1 + noise_2...
    noise_vars <- if(input$num_noise > 0) paste(paste0("noise_", 1:input$num_noise), collapse = " + ") else NULL
    formula_str <- if(is.null(noise_vars)) "sales ~ budget" else paste("sales ~ budget +", noise_vars)
    
    fit <- lm(as.formula(formula_str), data = full_df)
    s <- summary(fit)
    
    data.frame(
      Metric = c("Multiple R-Squared", "Adjusted R-Squared"),
      Value = c(s$r.squared, s$adj.r.squared)
    )
  })
  
  output$r2_table <- renderTable({ model_results() }, digits = 4)
  
  output$r2_comparison <- renderPlot({
    res <- model_results()
    ggplot(res, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", width = 0.5) +
      ylim(0, 1) +
      theme_minimal() +
      labs(title = paste("Model Fit with", input$num_noise, "Random Noise Variables"),
           y = "Score (0 to 1)", x = "") +
      scale_fill_manual(values = c("firebrick", "dodgerblue")) +
      theme(legend.position = "none", text = element_text(size = 15))
  })
}

shinyApp(ui, server)