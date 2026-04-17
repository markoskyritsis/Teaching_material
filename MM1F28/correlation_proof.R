library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Correlation Sampling Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample Size (n):", min = 5, max = 100, value = 30),
      sliderInput("noise", "Amount of Noise:", min = 0, max = 500, value = 150),
      actionButton("resimulate", "Generate New Null Distribution", class = "btn-primary")
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      plotOutput("nullDistPlot"),
      h4(textOutput("statsText"))
    )
  )
)

server <- function(input, output) {
  
  # 1. Create the 'Observed' data based on sliders
  observed_data <- reactive({
    x <- seq(1, 100, length.out = input$n)
    # The 'True' relationship is y = x, but we add the user's noise
    y <- x + rnorm(input$n, mean = 0, sd = input$noise)
    return(data.frame(x = x, y = y))
  })
  
  # 2. Generate the Null Distribution (The 'World of No Relationship')
  null_r_values <- eventReactive(input$resimulate, {
    # Pre-generate 10,000 correlations where x and y are independent
    replicate(10000, {
      s1 <- rnorm(input$n, 100, 40)
      s2 <- rnorm(input$n, 100, 40)
      cor(s1, s2)
    })
  }, ignoreNULL = FALSE)
  
  # Plot 1: The Scatterplot
  output$scatterPlot <- renderPlot({
    df <- observed_data()
    ggplot(df, aes(x, y)) + 
      geom_point(color = "blue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
      labs(title = "Observed Data", subtitle = paste("Actual r =", round(cor(df$x, df$y), 3)))
  })
  
  # Plot 2: The Null Distribution Histogram
  output$nullDistPlot <- renderPlot({
    rs <- null_r_values()
    actual_r <- cor(observed_data()$x, observed_data()$y)
    
    ggplot(data.frame(r = rs), aes(x = r)) +
      geom_histogram(bins = 50, fill = "gray", color = "white") +
      geom_vline(xintercept = c(actual_r, -actual_r), color = "red", linetype = "dashed", size = 1.5) +
      labs(title = "The Null Distribution (10,000 simulations of null hypothesis (no correlation))",
           subtitle = "Red lines show observed correlation (two-tailed)") +
      theme_minimal()
  })
  
  output$statsText <- renderText({
    rs <- null_r_values()
    actual_r <- cor(observed_data()$x, observed_data()$y)
    p_val <- sum(abs(rs) >= abs(actual_r)) / 10000
    
    official_test <- cor.test(observed_data()$x, observed_data()$y)
    p_val_official <- round(official_test$p.value, 4)
    
#    paste0("Simulated P-Value: ", p_val, " (Chance of seeing this result in a random world)")
    paste0("Simulated P-Value: ", p_val, "  |  Official (cor.test) P-Value: ", p_val_official)
  })
}

shinyApp(ui = ui, server = server)