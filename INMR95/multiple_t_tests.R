library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  titlePanel("Why We Don't Just Use Multiple t-tests"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Simulation Controls"),
      sliderInput("groups", 
                  "Number of Groups:", 
                  min = 2, max = 6, value = 2, step = 1),
      sliderInput("sims", 
                  "Number of Simulations:", 
                  min = 100, max = 5000, value = 1000, step = 100),
      sliderInput("sample_size",
                  "Sample Size per Group:",
                  min = 10, max = 100, value = 30, step = 10),
      hr(),
      helpText("This app simulates data where there is NO real difference between the groups (the Null Hypothesis is true for all groups)."),
      helpText("It runs all possible pairwise t-tests for each simulation to see how often we get at least one 'false positive' (p < 0.05).")
    ),
    
    mainPanel(
      h3("The Family-Wise Error Rate (FWER)"),
      p("When comparing more than two groups, running multiple t-tests inflates the probability of finding a significant result by pure chance."),
      hr(),
      h4(textOutput("summary_title")),
      textOutput("fwer_text"),
      br(),
      plotOutput("fwer_plot")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  
  # Reactive expression to run the simulations
  sim_data <- reactive({
    k <- input$groups
    n_sims <- input$sims
    n <- input$sample_size
    alpha <- 0.05
    
    # Calculate number of pairwise comparisons: k(k-1)/2
    num_comparisons <- choose(k, 2)
    
    # Run simulations
    # We replicate the process n_sims times
    results <- replicate(n_sims, {
      # Draw from standard normal distribution (Null hypothesis is TRUE)
      y <- rnorm(k * n)
      group <- factor(rep(1:k, each = n))
      
      # Perform all pairwise t-tests without any p-value adjustment
      # pairwise.t.test returns a matrix of p-values
      p_vals <- pairwise.t.test(y, group, p.adjust.method = "none")$p.value
      
      # Is there AT LEAST ONE Type I error (false positive) in this set of tests?
      any(p_vals < alpha, na.rm = TRUE)
    })
    
    # Calculate the empirical error rate
    empirical_error_rate <- mean(results)
    
    list(
      k = k,
      comparisons = num_comparisons,
      empirical = empirical_error_rate
    )
  })
  
  output$summary_title <- renderText({
    paste("Results over", input$sims, "Simulations")
  })
  
  output$fwer_text <- renderText({
    data <- sim_data()
    paste0("With ", data$k, " groups, there are ", data$comparisons, 
           " possible pairwise comparisons. Even though our alpha for a single test is set to 5%, the empirical Family-Wise Error Rate (the chance of making AT LEAST ONE false discovery) is ", 
           round(data$empirical * 100, 1), "%.")
  })
  
  output$fwer_plot <- renderPlot({
    data <- sim_data()
    
    # Dataframe for plotting
    df <- data.frame(
      Type = c("Nominal Alpha\n(Single Test)", "Empirical FWER\n(All Pairwise Tests)"),
      ErrorRate = c(0.05, data$empirical)
    )
    
    # Lock the factor levels so the plot order makes sense
    df$Type <- factor(df$Type, levels = c("Nominal Alpha\n(Single Test)", "Empirical FWER\n(All Pairwise Tests)"))
    
    ggplot(df, aes(x = Type, y = ErrorRate, fill = Type)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 1) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                         limits = c(0, max(0.5, data$empirical + 0.1))) +
      labs(title = paste("False Positive Rate with", data$k, "Groups"),
           subtitle = "Red dashed line indicates the 5% threshold",
           y = "Probability of ≥ 1 False Positive",
           x = "") +
      theme_minimal() +
      theme(legend.position = "none", 
            text = element_text(size = 14),
            plot.title = element_text(face = "bold")) +
      scale_fill_manual(values = c("Nominal Alpha\n(Single Test)" = "#B0C4DE", 
                                   "Empirical FWER\n(All Pairwise Tests)" = "#FF7F50"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)