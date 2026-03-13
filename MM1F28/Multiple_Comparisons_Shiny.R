library(shiny)

ui <- fluidPage(
  titlePanel("The Danger of Multiple Comparisons"),
  
  sidebarLayout(
    sidebarPanel(
      p("When we compare many groups, the chance of finding at least one 'significant' 
        p-value by pure luck increases."),
      sliderInput("num_groups", "Number of Groups (Samples):", 
                  min = 2, max = 10, value = 3),
      numericInput("iterations", "Number of Simulations:", 
                   value = 100, min = 10, max = 500),
      actionButton("run", "Run Simulation", class = "btn-primary")
    ),
    
    mainPanel(
      h4("Results"),
      textOutput("error_count"),
      hr(),
      plotOutput("distPlot"),
      p("Note: Every sample is drawn from the SAME population. 
        Any 'significant' result is a Type I Error (a false positive).")
    )
  )
)

server <- function(input, output) {
  
  # Reactive calculation triggered by the button
  results <- eventReactive(input$run, {
    pop <- rnorm(100000, 100, 30)
    total_type_1_errors <- 0
    
    for (i in 1:input$iterations) {
      # Create a list of samples
      samples <- replicate(input$num_groups, sample(pop, 30), simplify = FALSE)
      
      # Perform all possible pairwise t-tests
      combos <- combn(input$num_groups, 2)
      
      significant_found_in_round <- FALSE
      for (j in 1:ncol(combos)) {
        g1 <- combos[1, j]
        g2 <- combos[2, j]
        
        p_val <- t.test(samples[[g1]], samples[[g2]], var.equal = TRUE)$p.value
        if (p_val < 0.05) {
          significant_found_in_round <- TRUE
          break # We found at least one error in this experiment
        }
      }
      
      if (significant_found_in_round) {
        total_type_1_errors <- total_type_1_errors + 1
      }
    }
    
    # Calculate the percentage
    (total_type_1_errors / input$iterations) * 100
  })
  
  output$error_count <- renderText({
    paste0("Percentage of simulations where at least ONE false positive was found: ", 
           results(), "%")
  })
  
  output$distPlot <- renderPlot({
    # Simple bar chart comparing observed error rate vs expected 5%
    barplot(c(5, results()), 
            names.arg = c("Expected (1 Test)", paste(input$num_groups, "Groups")),
            col = c("grey", "firebrick"),
            ylab = "Type I Error Rate (%)",
            main = "Inflation of Error Rates")
    abline(h = 5, lty = 2)
  })
}

shinyApp(ui = ui, server = server)