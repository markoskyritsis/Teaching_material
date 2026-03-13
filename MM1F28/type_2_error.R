library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Type II Error: The 'Missed' Discovery"),
  
  sidebarLayout(
    sidebarPanel(
      p("Type II Error (Beta) occurs when there IS a real difference, 
        but our test fails to find it (p > Alpha)."),
      hr(),
      sliderInput("mu2", "Mean of Group 2 (Group 1 is 100):", 
                  min = 100, max = 150, value = 115),
      sliderInput("n_size", "Sample Size (n):", 
                  min = 5, max = 200, value = 30),
      sliderInput("alpha", "Significance Level (Alpha):", 
                  min = 0.01, max = 0.10, value = 0.05, step = 0.01),
      numericInput("num_sims", "Simulations:", value = 500, min = 100, max = 1000),
      actionButton("run", "Run Simulation", class = "btn-warning", style="width:100%")
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("distPlot", height = "300px"))
      ),
      hr(),
      fluidRow(
        column(6, 
               wellPanel(
                 h4("Statistical Outcomes"),
                 textOutput("type2_rate"),
                 textOutput("power_rate")
               )),
        column(6,
               p(strong("The Trade-off:")),
               p("Notice that if you decrease Alpha (to be stricter), your Type II error rate goes UP. 
                 This is why we can't just set Alpha to 0.0001!")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  results <- eventReactive(input$run, {
    type2_count <- 0
    
    for (i in 1:input$num_sims) {
      # Draw from two DIFFERENT populations
      s1 <- rnorm(input$n_size, 100, 30)
      s2 <- rnorm(input$n_size, input$mu2, 30)
      
      p_val <- t.test(s1, s2, var.equal = TRUE)$p.value
      if (p_val > input$alpha) {
        type2_count <- type2_count + 1
      }
    }
    
    rate <- (type2_count / input$num_sims)
    list(rate = rate, power = 1 - rate)
  })
  
  output$distPlot <- renderPlot({
    # Visualize the two populations
    x <- seq(0, 250, length.out = 200)
    y1 <- dnorm(x, 100, 30)
    y2 <- dnorm(x, input$mu2, 30)
    
    df <- data.frame(x = rep(x, 2), y = c(y1, y2), 
                     Group = rep(c("Group 1", "Group 2"), each = 200))
    
    ggplot(df, aes(x = x, y = y, fill = Group)) +
      geom_area(alpha = 0.5, position = "identity") +
      labs(title = "Population Overlap (The Source of Type II Error)",
           subtitle = "More overlap = Higher Type II Error",
           x = "Value", y = "Density") +
      theme_minimal() +
      scale_fill_manual(values = c("steelblue", "orange"))
  })
  
  output$type2_rate <- renderText({
    paste0("Type II Error Rate (Beta): ", round(results()$rate * 100, 1), "%")
  })
  
  output$power_rate <- renderText({
    paste0("Statistical Power (1 - Beta): ", round(results()$power * 100, 1), "%")
  })
}

shinyApp(ui, server)