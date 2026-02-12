library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Visualizing the Law of Large Numbers"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pop_mean", "Population Mean:", 100),
      sliderInput("pop_sd", "Population Std Dev (Sigma):", min = 1, max = 50, value = 15),
      sliderInput("n_size", "Number of Samples:", min = 10, max = 2000, value = 100, step = 10)
    ),
    mainPanel(
      plotOutput("llnPlot"),
      textOutput("finalMean")
    )
  )
)

server <- function(input, output) {
  output$llnPlot <- renderPlot({
    # Generate random data based on inputs
    set.seed(123) 
    samples <- rnorm(input$n_size, mean = input$pop_mean, sd = input$pop_sd)
    
    # Calculate running mean
    running_mean <- cumsum(samples) / seq_along(samples)
    df <- data.frame(Trial = 1:input$n_size, Mean = running_mean)
    
    ggplot(df, aes(x = Trial, y = Mean)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_hline(yintercept = input$pop_mean, linetype = "dashed", color = "red") +
      # This is the fix: it keeps the zoom level constant
      coord_cartesian(ylim = c(input$pop_mean - 30, input$pop_mean + 30)) + 
      labs(title = "Running Sample Mean vs. Population Mean",
           subtitle = "With a fixed Y-axis, you can now see the 'noise' increase",
           x = "Number of Observations", y = "Cumulative Average") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)