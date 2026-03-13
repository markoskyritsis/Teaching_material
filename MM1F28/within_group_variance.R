library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Understanding 'Within-Group' Variance (The Noise)"),
  
  sidebarLayout(
    sidebarPanel(
      p("The F-ratio compares the variation BETWEEN groups to the variation WITHIN groups."),
      p("Here, we calculate the variance of each group and take the average. This is our 'Denominator' or error term."),
      sliderInput("n_size", "Sample Size (n) per group:", 
                  min = 5, max = 500, value = 30, step = 5),
      sliderInput("pop_sd", "Population Standard Deviation (Sigma):", 
                  min = 1, max = 100, value = 30),
      hr(),
      helpText("We are calculating Mean(Var1, Var2, Var3) across 1,000 simulations.")
    ),
    
    mainPanel(
      h4("Sampling Distribution of the Mean of Variances"),
      plotOutput("withinPlot"),
      tableOutput("statsTable"),
      p("Observation: Unlike the Variance of Means (which shrinks to 0 as n increases), 
        the Mean of Variances targets the ACTUAL population variance (Sigma squared)."),
      p(strong("True Population Variance:"), textOutput("true_var", inline = TRUE))
    )
  )
)

server <- function(input, output) {
  
  sim_data <- reactive({
    # Each simulation takes 3 samples and finds the mean of their variances
    means_of_vars <- replicate(1000, {
      s1 <- rnorm(input$n_size, 100, input$pop_sd)
      s2 <- rnorm(input$n_size, 100, input$pop_sd)
      s3 <- rnorm(input$n_size, 100, input$pop_sd)
      mean(c(var(s1), var(s2), var(s3)))
    })
    
    data.frame(val = means_of_vars)
  })
  
  output$true_var <- renderText({
    input$pop_sd^2
  })
  
  output$withinPlot <- renderPlot({
    true_v <- input$pop_sd^2
    ggplot(sim_data(), aes(x = val)) +
      geom_histogram(fill = "seagreen", color = "white", bins = 30) +
      geom_vline(xintercept = true_v, color = "orange", linetype = "solid", size = 1.2) +
      labs(title = "Distribution of Mean Within-Group Variance",
           subtitle = "The orange line is the true Population Variance (Sigma^2)",
           x = "Mean of Sample Variances",
           y = "Frequency") +
      theme_minimal()
  })
  
  output$statsTable <- renderTable({
    df <- sim_data()
    data.frame(
      Metric = c("Expected Noise (Population Variance)", "Simulated Average Within-Group Var"),
      Value = c(input$pop_sd^2, mean(df$val))
    )
  })
}

shinyApp(ui = ui, server = server)