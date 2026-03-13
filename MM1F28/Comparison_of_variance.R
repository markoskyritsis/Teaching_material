library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Why Variance of Means Matters"),
  
  sidebarLayout(
    sidebarPanel(
      p("If groups are identical, the variance between their means should be 0."),
      p("But sampling error makes it > 0. Let's see how Sample Size (n) affects this."),
      sliderInput("n_size", "Sample Size (n) per group:", 
                  min = 5, max = 500, value = 30, step = 5),
      hr(),
      helpText("We are taking 3 samples from the same population 1,000 times.")
    ),
    
    mainPanel(
      h4("Sampling Distribution of Var(Means)"),
      plotOutput("varPlot"),
      tableOutput("statsTable"),
      p("Notice: As 'n' increases, the variance between means gets closer to zero. 
        In ANOVA, if our observed variance is much LARGER than this 'sampling noise', 
        we conclude the groups are actually different.")
    )
  )
)

server <- function(input, output) {
  
  # Generate 1,000 simulations of var(mean1, mean2, mean3)
  sim_data <- reactive({
    pop_mean <- 100
    pop_sd <- 30
    
    # Pre-allocate vector for speed
    variances <- replicate(1000, {
      m1 <- mean(rnorm(input$n_size, pop_mean, pop_sd))
      m2 <- mean(rnorm(input$n_size, pop_mean, pop_sd))
      m3 <- mean(rnorm(input$n_size, pop_mean, pop_sd))
      var(c(m1, m2, m3))
    })
    
    data.frame(var_means = variances)
  })
  
  output$varPlot <- renderPlot({
    ggplot(sim_data(), aes(x = var_means)) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30) +
      geom_vline(aes(xintercept = mean(var_means)), color = "red", linetype = "dashed", size = 1) +
      labs(title = paste("Distribution of Variance between 3 Means (n =", input$n_size, ")"),
           x = "Calculated Variance of Means",
           y = "Frequency") +
      theme_minimal()
  })
  
  output$statsTable <- renderTable({
    df <- sim_data()
    data.frame(
      Metric = c("Average Variance (Sampling Noise)", "Minimum Observed", "Maximum Observed"),
      Value = c(mean(df$var_means), min(df$var_means), max(df$var_means))
    )
  })
}

shinyApp(ui = ui, server = server)