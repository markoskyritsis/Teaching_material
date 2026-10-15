library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("The Studentized Range Distribution (q)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Simulation Parameters"),
      sliderInput("k", "Number of Groups (k):", min = 2, max = 15, value = 3, step = 1),
      sliderInput("n", "Sample Size per Group (n):", min = 5, max = 50, value = 20, step = 1),
      
      hr(),
      actionButton("resample", "Run 2,000 Simulations", class = "btn-primary", width = "100%"),
      br(),br(),
      helpText("This simulates drawing k groups from the same exact population (the Null Hypothesis is true)."),
      helpText("For every simulation, we find the highest mean, subtract the lowest mean, and divide by the standard error to get the q-statistic.")
    ),
    
    mainPanel(
      h3("Expected Maximum Spread"),
      p("As you increase the number of groups, the chance of randomly drawing extreme means increases. The q-distribution stretches to account for this."),
      plotOutput("q_plot", height = "400px"),
      hr(),
      uiOutput("q_text")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  
  sim_q <- reactive({
    input$resample
    
    k <- input$k
    n <- input$n
    sims <- 2000
    
    # Simulate the q-statistic 2,000 times
    q_vals <- replicate(sims, {
      # 1. Draw k * n values from a standard normal distribution N(0,1)
      y <- rnorm(k * n)
      group <- factor(rep(1:k, each = n))
      
      # 2. Calculate group means and variances
      means <- tapply(y, group, mean)
      vars <- tapply(y, group, var)
      
      # 3. Find max difference
      max_diff <- max(means) - min(means)
      
      # 4. Calculate pooled variance (Mean Square Within)
      msw <- mean(vars)
      
      # 5. Calculate q
      # Note: The standard error for q uses msw/n, without the sqrt(2) used in t-tests
      se <- sqrt(msw / n)
      max_diff / se
    })
    
    # Calculate the theoretical critical value for alpha = 0.05
    df_within <- k * (n - 1)
    crit_val <- qtukey(0.95, nmeans = k, df = df_within)
    
    list(q_vals = q_vals, crit_val = crit_val, k = k, n = n, df = df_within)
  })
  
  output$q_plot <- renderPlot({
    res <- sim_q()
    
    df_plot <- data.frame(q = res$q_vals)
    
    # Dynamically set x-axis limits so the shift is visually obvious
    max_x <- max(8, max(res$q_vals) + 1)
    
    ggplot(df_plot, aes(x = q)) +
      geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#9b59b6", color = "white") +
      geom_vline(xintercept = res$crit_val, color = "black", linewidth = 1.5, linetype = "dashed") +
      annotate("text", x = res$crit_val + 0.15, y = 0.3, 
               label = paste("Top 5% Threshold (q =", round(res$crit_val, 2), ")"), 
               color = "black", angle = 90, vjust = 0) +
      labs(title = paste("Simulated q-Distribution for k =", res$k, "groups"),
           x = "Studentized Range Statistic (q)",
           y = "Density") +
      theme_minimal() +
      theme(text = element_text(size = 14)) +
      coord_cartesian(xlim = c(0, max_x))
  })
  
  output$q_text <- renderUI({
    res <- sim_q()
    
    # For comparison, calculate what a standard t-test critical value would be
    # q is related to t by a factor of sqrt(2). So equivalent q for a t-test is t_crit * sqrt(2)
    t_crit <- qt(0.975, df = res$df)
    equiv_q_from_t <- t_crit * sqrt(2)
    
    HTML(paste0(
      "<div style='font-size: 16px; font-family: monospace; background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
      "<b>Number of Groups (k):</b> ", res$k, "<br/>",
      "<b>Theoretical 5% Critical Value (Tukey's q):</b> <span style='color: #9b59b6; font-weight: bold;'>", round(res$crit_val, 2), "</span><br/><br/>",
      "<i>If we just used a standard uncorrected t-test, the threshold would stay fixed at roughly <b>", round(equiv_q_from_t, 2), "</b>. ",
      "Notice how Tukey's threshold pushes further to the right to protect against the inflated false positive rate of having ", res$k, " groups.</i>",
      "</div>"
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)