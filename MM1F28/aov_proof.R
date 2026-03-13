library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("The Grand Finale: Monte Carlo vs. Formal ANOVA"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Simulation Parameters"),
      sliderInput("n_size", "Sample Size (n) per group:", min = 5, max = 100, value = 30),
      sliderInput("num_sims", "Number of Monte Carlo Iterations:", 
                  min = 1000, max = 10000, value = 2000, step = 1000),
      
      hr(),
      h4("Test Data (Set the Means)"),
      helpText("Change these to see how the F-ratio reacts!"),
      sliderInput("mu1", "Mean Group A:", min = 80, max = 120, value = 100),
      sliderInput("mu2", "Mean Group B:", min = 80, max = 120, value = 100),
      sliderInput("mu3", "Mean Group C:", min = 80, max = 120, value = 100),
      
      actionButton("run", "Run Simulation", class = "btn-success", style="width: 100%")
    ),
    
    mainPanel(
      fluidRow(
        column(6, h4("Your Simulated Null Distribution"), plotOutput("distPlot")),
        column(6, h4("Your Sample Data"), plotOutput("boxPlot"))
      ),
      hr(),
      fluidRow(
        column(6, 
               h4("Manual Calculation (Sim-style)"),
               wellPanel(
                 textOutput("manual_f"),
                 textOutput("manual_p")
               )),
        column(6, 
               h4("Official R ANOVA Result"),
               wellPanel(
                 verbatimTextOutput("anova_summary")
               ))
      ),
      hr(),
      tags$small("Note: The manual F-ratio is scaled by 'n' to match the official Mean Square Between (MSB).")
    )
  )
)

server <- function(input, output) {
  
  # 1. Generate the Null Distribution (The "What if they were all the same" world)
  null_f_ratios <- eventReactive(input$run, {
    withProgress(message = 'Running Monte Carlo...', value = 0, {
      replicate(input$num_sims, {
        # Samples from SAME population (Null is true)
        s1 <- rnorm(input$n_size, 100, 30)
        s2 <- rnorm(input$n_size, 100, 30)
        s3 <- rnorm(input$n_size, 100, 30)
        
        # Calculate Scaled F: (Var of means * n) / (Mean of variances)
        (var(c(mean(s1), mean(s2), mean(s3))) * input$n_size) / mean(c(var(s1), var(s2), var(s3)))
      })
    })
  })
  
  # 2. Generate the "Real" Test Data based on sliders
  test_data <- reactive({
    data.frame(
      Value = c(rnorm(input$n_size, input$mu1, 30),
                rnorm(input$n_size, input$mu2, 30),
                rnorm(input$n_size, input$mu3, 30)),
      Group = factor(rep(c("A", "B", "C"), each = input$n_size))
    )
  })
  
  # Calculate observed F-ratio for the test data
  observed_f <- reactive({
    df <- test_data()
    means <- aggregate(Value ~ Group, df, mean)$Value
    vars <- aggregate(Value ~ Group, df, var)$Value
    
    (var(means) * input$n_size) / mean(vars)
  })
  
  output$distPlot <- renderPlot({
    f_vals <- null_f_ratios()
    obs_f <- observed_f()
    
    ggplot(data.frame(f = f_vals), aes(x = f)) +
      geom_density(fill = "grey90") +
      geom_vline(xintercept = obs_f, color = "red", size = 1.2) +
      labs(title = "F-Distribution (Null)", x = "F-ratio", y = "Density") +
      theme_minimal()
  })
  
  output$boxPlot <- renderPlot({
    ggplot(test_data(), aes(x = Group, y = Value, fill = Group)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Group Comparisons")
  })
  
  output$manual_f <- renderText({
    paste("Simulated F-ratio:", round(observed_f(), 4))
  })
  
  output$manual_p <- renderText({
    p_val <- sum(null_f_ratios() >= observed_f()) / input$num_sims
    paste("Simulated P-value:", p_val)
  })
  
  output$anova_summary <- renderPrint({
    summary(aov(Value ~ Group, data = test_data()))
  })
}

shinyApp(ui = ui, server = server)