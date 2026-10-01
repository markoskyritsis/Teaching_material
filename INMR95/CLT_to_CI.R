library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Confidence Intervals: The 95% Hit Rate"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pop_mean", "True Population Mean:", 100),
      sliderInput("pop_sd", "Population SD (sigma):", min = 1, max = 20, value = 10),
      sliderInput("n_size", "Sample Size (n):", min = 5, max = 100, value = 30),
      actionButton("resimulate", "Take 100 New Samples"),
      hr(),
      helpText("Each line represents one sample's 95% Confidence Interval (Mean ± 1.96 * SE).")
    ),
    mainPanel(
      plotOutput("ciPlot", height = "600px"),
      wellPanel(textOutput("hit_rate"))
    )
  )
)

server <- function(input, output) {
  
  # Reactive simulation: Creates 100 samples and their intervals
  ci_data <- eventReactive(input$resimulate, {
    n_intervals <- 100
    z_score <- 1.96 # For 95% confidence
    
    results <- data.frame(id = 1:n_intervals)
    
    # Generate 100 samples and calculate bounds
    plot_data <- replicate(n_intervals, {
      samp <- rnorm(input$n_size, mean = input$pop_mean, sd = input$pop_sd)
      m <- mean(samp)
      se <- sd(samp) / sqrt(input$n_size)
      lower <- m - (z_score * se)
      upper <- m + (z_score * se)
      c(m, lower, upper)
    })
    
    results$mean <- plot_data[1,]
    results$lower <- plot_data[2,]
    results$upper <- plot_data[3,]
    
    # Check if the true mean is captured
    results$captured <- results$lower <= input$pop_mean & results$upper >= input$pop_mean
    return(results)
  }, ignoreNULL = FALSE)
  
  output$ciPlot <- renderPlot({
    df <- ci_data()
    
    # Calculate a fixed range based on population mean and SD
    # This ensures the window doesn't "jump" when n changes
    window_range <- input$pop_sd * 1.5 
    
    ggplot(df, aes(x = id, y = mean, color = captured)) +
      geom_point(size = 1.5) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
      geom_hline(yintercept = input$pop_mean, linetype = "dashed", color = "black", size = 1) +
      coord_flip(ylim = c(input$pop_mean - window_range, input$pop_mean + window_range)) + 
      scale_color_manual(values = c("TRUE" = "seagreen", "FALSE" = "firebrick")) +
      labs(title = "100 Samples: Do they capture the truth?",
           subtitle = paste("Fixed scale: Mean +/-", window_range),
           x = "Sample Number", y = "Value (Sample Mean & CI)",
           color = "Captures True Mean?") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), # Hide sample IDs to declutter
            axis.ticks.y = element_blank())
  })
}

shinyApp(ui = ui, server = server)