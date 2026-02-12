library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("The Central Limit Theorem in Action"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Population Distribution:", 
                  choices = c("Normal", "Uniform", "Exponential")),
      sliderInput("n_size", "Sample Size (n):", min = 2, max = 100, value = 10),
      sliderInput("pop_sd", "Population SD (sigma):", min = 1, max = 20, value = 5),
      numericInput("n_trials", "Number of Trials (Simulations):", 1000),
      hr(),
      wellPanel(
        h4("Statistics"),
        textOutput("stats_output")
      )
    ),
    mainPanel(
      plotOutput("cltPlot"),
      helpText("The distribution of sample means will become normal as 'n' increases.")
    )
  )
)

server <- function(input, output) {
  
  # Reactive calculation of means
  sample_means <- reactive({
    # We adjust the population generation based on the choice
    # Note: To keep SD constant across types, we'd need more complex math,
    # so we'll focus on Normal for the SD/SE comparison.
    data <- replicate(input$n_trials, {
      if(input$dist == "Normal") {
        s <- rnorm(input$n_size, mean = 50, sd = input$pop_sd)
      } else if(input$dist == "Uniform") {
        # Adjusted to roughly match spread
        s <- runif(input$n_size, min = 50 - (input$pop_sd * 1.7), max = 50 + (input$pop_sd * 1.7))
      } else {
        s <- rexp(input$n_size, rate = 1/50)
      }
      mean(s)
    })
    return(data)
  })
  
  output$cltPlot <- renderPlot({
    df <- data.frame(means = sample_means())
    
    ggplot(df, aes(x = means)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "seagreen", color = "white") +
      stat_function(fun = dnorm, args = list(mean = mean(df$means), sd = sd(df$means)), 
                    color = "red", size = 1) +
      labs(title = "Distribution of Sample Means",
           x = "Sample Mean Value", y = "Density") +
      theme_minimal()
  })
  
  output$stats_output <- renderText({
    # We need the actual raw samples to get a single sample SD
    set.seed(123) # For consistency in the example
    single_sample <- if(input$dist == "Normal") {
      rnorm(input$n_size, mean = 50, sd = input$pop_sd)
    } else if(input$dist == "Uniform") {
      runif(input$n_size, min = 50 - (input$pop_sd * 1.7), max = 50 + (input$pop_sd * 1.7))
    } else {
      rexp(input$n_size, rate = 1/50)
    }
    
    means_vec <- sample_means()
    obs_se <- sd(means_vec)
    theory_se <- input$pop_sd / sqrt(input$n_size)
    est_se <- sd(single_sample) / sqrt(input$n_size)
    
    paste0("1. Theoretical SE (using pop sigma): ", round(theory_se, 3),
           "\n2. Observed SD of all Means: ", round(obs_se, 3),
           "\n3. Estimated SE (from ONE sample 's'): ", round(est_se, 3))
  })
}

shinyApp(ui = ui, server = server)