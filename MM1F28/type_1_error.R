library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Type I Error: The Predictable Mistake"),
  
  sidebarLayout(
    sidebarPanel(
      p("A Type I error occurs when we find a 'significant' difference between groups 
        that are actually identical."),
      hr(),
      sliderInput("alpha", "Significance Level (Alpha):", 
                  min = 0.01, max = 0.20, value = 0.05, step = 0.01),
      sliderInput("n_size", "Sample Size (n):", 
                  min = 5, max = 100, value = 20),
      numericInput("num_sims", "Number of Simulations:", 
                   value = 500, min = 100, max = 2000),
      actionButton("run", "Run Simulation", class = "btn-primary", style="width:100%")
    ),
    
    mainPanel(
      h4("Simulation Results"),
      plotOutput("errorPlot"),
      wellPanel(
        h3(textOutput("final_rate")),
        p("The theoretical target error rate is: ", textOutput("target_alpha", inline = TRUE))
      ),
      p(tags$small("The blue line shows the running average of errors. 
                   Notice how it stabilizes as we run more simulations."))
    )
  )
)

server <- function(input, output) {
  
  sim_results <- eventReactive(input$run, {
    errors <- numeric(input$num_sims)
    
    # Run the simulation
    for (i in 1:input$num_sims) {
      # Draw from same population (Null is TRUE)
      s1 <- rnorm(input$n_size, 100, 30)
      s2 <- rnorm(input$n_size, 100, 30)
      
      p_val <- t.test(s1, s2, var.equal = TRUE)$p.value
      errors[i] <- ifelse(p_val < input$alpha, 1, 0)
    }
    
    # Calculate cumulative error rate
    data.frame(
      trial = 1:input$num_sims,
      cumulative_rate = cumsum(errors) / (1:input$num_sims)
    )
  })
  
  output$errorPlot <- renderPlot({
    df <- sim_results()
    ggplot(df, aes(x = trial, y = cumulative_rate)) +
      geom_line(color = "steelblue", size = 1) +
      geom_hline(yintercept = input$alpha, color = "red", linetype = "dashed") +
      annotate("text", x = input$num_sims*0.8, y = input$alpha, 
               label = "Target Alpha", vjust = -1, color = "red") +
      labs(title = "Stability of Type I Error Rate",
           x = "Number of Experiments Run",
           y = "Observed Error Rate") +
      ylim(0, max(df$cumulative_rate, input$alpha + 0.05)) +
      theme_minimal()
  })
  
  output$final_rate <- renderText({
    df <- sim_results()
    final <- round(tail(df$cumulative_rate, 1) * 100, 2)
    paste0("Observed Error Rate: ", final, "%")
  })
  
  output$target_alpha <- renderText({
    paste0(input$alpha * 100, "%")
  })
}

shinyApp(ui, server)