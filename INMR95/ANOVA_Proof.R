library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("The F-Distribution and the P-Value"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Observed Data (Reality)"),
      sliderInput("mean_A", "Group A Mean:", min = 40, max = 60, value = 45, step = 1),
      sliderInput("mean_B", "Group B Mean:", min = 40, max = 60, value = 50, step = 1),
      sliderInput("mean_C", "Group C Mean:", min = 40, max = 60, value = 55, step = 1),
      
      hr(),
      h4("Shared Parameters"),
      sliderInput("sd", "Standard Deviation:", min = 1, max = 15, value = 5, step = 0.5),
      sliderInput("n", "Sample Size (per group):", min = 5, max = 50, value = 20, step = 1),
      
      hr(),
      actionButton("resample", "Draw New Observed Sample", class = "btn-primary", width = "100%"),
      br(),br(),
      helpText("The simulation runs 2,000 times under the Null Hypothesis (all group means equal) to build the distribution.")
    ),
    
    mainPanel(
      plotOutput("dist_plot", height = "400px"),
      hr(),
      h3("Comparing P-Values"),
      uiOutput("results")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  
  # Reactive expression to run simulations
  sim_results <- reactive({
    # Depend on the action button to allow resampling without changing sliders
    input$resample 
    
    n <- input$n
    sd_val <- input$sd
    
    # 1. Generate Observed Data (Reality)
    # This uses the user-defined means which may or may not be equal
    obs_data <- data.frame(
      val = c(rnorm(n, input$mean_A, sd_val),
              rnorm(n, input$mean_B, sd_val),
              rnorm(n, input$mean_C, sd_val)),
      group = factor(rep(c("A", "B", "C"), each = n))
    )
    
    # Calculate Observed F and Analytical p-value
    obs_model <- aov(val ~ group, data = obs_data)
    obs_F <- summary(obs_model)[[1]][["F value"]][1]
    analytical_p <- summary(obs_model)[[1]][["Pr(>F)"]][1]
    
    # 2. Generate Null Distribution (Simulation)
    # We force the means to be identical to simulate the Null Hypothesis
    n_sims <- 2000
    null_F <- replicate(n_sims, {
      sim_val <- rnorm(n * 3, mean = 50, sd = sd_val)
      sim_group <- factor(rep(c("A", "B", "C"), each = n))
      sim_model <- aov(sim_val ~ sim_group)
      summary(sim_model)[[1]][["F value"]][1]
    })
    
    # Calculate Empirical p-value
    empirical_p <- mean(null_F >= obs_F)
    
    # Degrees of freedom for the theoretical curve
    df1 <- 2
    df2 <- (n * 3) - 3
    
    list(obs_F = obs_F, null_F = null_F, 
         emp_p = empirical_p, ana_p = analytical_p,
         df1 = df1, df2 = df2)
  })
  
  output$dist_plot <- renderPlot({
    res <- sim_results()
    plot_data <- data.frame(F_val = res$null_F)
    
    # Determine plot limits dynamically
    max_x <- max(10, max(res$null_F), res$obs_F + 2)
    
    ggplot(plot_data, aes(x = F_val)) +
      # Empirical histogram
      geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "#B0C4DE", color = "white") +
      # Theoretical F-distribution curve
      stat_function(fun = df, args = list(df1 = res$df1, df2 = res$df2), 
                    color = "blue", linewidth = 1, linetype = "dashed") +
      # Observed F-statistic line
      geom_vline(xintercept = res$obs_F, color = "red", linewidth = 1.5) +
      annotate("text", x = res$obs_F + (max_x * 0.02), y = 0.5, 
               label = "Observed F", color = "red", angle = 90, vjust = 0) +
      labs(title = "Simulated Null Distribution vs. Theoretical F-Curve",
           subtitle = "Dashed blue line = Theoretical F-distribution. Red line = Observed F-statistic.",
           x = "F-Statistic", y = "Density") +
      theme_minimal() +
      theme(text = element_text(size = 14)) +
      coord_cartesian(xlim = c(0, max_x))
  })
  
  output$results <- renderUI({
    res <- sim_results()
    
    HTML(paste0(
      "<div style='font-size: 16px; font-family: monospace; background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
      "<b>Observed F-Statistic:</b> ", round(res$obs_F, 3), "<br/><br/>",
      "<b>Empirical p-value (from simulation):</b> <span style='color: #2c3e50; font-weight: bold;'>", round(res$emp_p, 4), "</span><br/>",
      "<i>(Proportion of the 2,000 simulated null F-values &ge; our observed F)</i><br/><br/>",
      "<b>Analytical p-value (from aov() function):</b> <span style='color: blue; font-weight: bold;'>", round(res$ana_p, 4), "</span><br/>",
      "<i>(Calculated using the exact mathematical F-distribution)</i>",
      "</div>"
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)