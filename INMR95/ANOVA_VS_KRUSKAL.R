library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Power Showdown: Unequal Variances & Unequal n"),
  
  sidebarLayout(
    sidebarPanel(
      # Group the sliders using layout containers to save vertical space
      fluidRow(
        column(4, 
               h4("Means"),
               sliderInput("mean_A", "Grp A:", min = 40, max = 60, value = 50, step = 1),
               sliderInput("mean_B", "Grp B:", min = 40, max = 60, value = 50, step = 1),
               sliderInput("mean_C", "Grp C:", min = 40, max = 60, value = 50, step = 1)
        ),
        column(4, 
               h4("Std Dev"),
               sliderInput("sd_A", "Grp A:", min = 1, max = 20, value = 5, step = 1),
               sliderInput("sd_B", "Grp B:", min = 1, max = 20, value = 5, step = 1),
               sliderInput("sd_C", "Grp C:", min = 1, max = 20, value = 5, step = 1)
        ),
        column(4, 
               h4("Sample Size"),
               sliderInput("n_A", "Grp A:", min = 5, max = 100, value = 20, step = 5),
               sliderInput("n_B", "Grp B:", min = 5, max = 100, value = 20, step = 5),
               sliderInput("n_C", "Grp C:", min = 5, max = 100, value = 20, step = 5)
        )
      ),
      
      hr(),
      actionButton("run_sim", "Run 500 Simulations", class = "btn-danger", width = "100%"),
      br(),br(),
      helpText("This app generates 500 datasets based on your settings, runs all three tests on every dataset, and counts how often each test successfully detects a difference (p < 0.05).")
    ),
    
    mainPanel(
      h3("Detection Rate"),
      p("If all means are equal (Null is true), bars show Type I Error (should be ~5%). If means differ, bars show Statistical Power."),
      plotOutput("power_plot", height = "400px"),
      hr(),
      uiOutput("power_text")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  
  sim_results <- reactive({
    input$run_sim
    
    isolate({
      mA <- input$mean_A; mB <- input$mean_B; mC <- input$mean_C
      sdA <- input$sd_A; sdB <- input$sd_B; sdC <- input$sd_C
      nA <- input$n_A; nB <- input$n_B; nC <- input$n_C
      
      sims <- 500
      
      results <- replicate(sims, {
        y <- c(rnorm(nA, mA, sdA), rnorm(nB, mB, sdB), rnorm(nC, mC, sdC))
        g <- factor(rep(c("A", "B", "C"), times = c(nA, nB, nC)))
        
        # 1. Standard ANOVA
        p_aov <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
        
        # 2. Welch ANOVA 
        p_welch <- oneway.test(y ~ g, var.equal = FALSE)$p.value
        
        # 3. Kruskal-Wallis
        p_kw <- kruskal.test(y ~ g)$p.value
        
        c(p_aov, p_welch, p_kw)
      })
      
      power_rates <- rowMeans(results < 0.05)
      names(power_rates) <- c("Standard ANOVA", "Welch ANOVA", "Kruskal-Wallis")
      
      list(rates = power_rates, means = c(mA, mB, mC))
    })
  })
  
  output$power_plot <- renderPlot({
    res <- sim_results()
    
    df <- data.frame(
      Test = factor(names(res$rates), levels = c("Standard ANOVA", "Welch ANOVA", "Kruskal-Wallis")),
      Rate = res$rates
    )
    
    # Dynamic y-axis to handle massive Type I error inflation
    max_y <- max(0.15, max(df$Rate) + 0.05)
    
    ggplot(df, aes(x = Test, y = Rate, fill = Test)) +
      geom_bar(stat = "identity", color = "black", width = 0.6) +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "black", linewidth = 1) +
      geom_text(aes(label = scales::percent(Rate, accuracy = 1)), vjust = -0.5, size = 6, fontface = "bold") +
      scale_y_continuous(labels = scales::percent, limits = c(0, max_y)) +
      labs(title = "Detection Rate (Power vs. Type I Error)",
           subtitle = "Dashed line marks the 5% alpha threshold",
           x = "", y = "Proportion of Significant Results (p < 0.05)") +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 16)) +
      scale_fill_manual(values = c("Standard ANOVA" = "#3498db", 
                                   "Welch ANOVA" = "#2ecc71", 
                                   "Kruskal-Wallis" = "#e74c3c"))
  })
  
  output$power_text <- renderUI({
    res <- sim_results()
    null_true <- length(unique(res$means)) == 1
    
    if (null_true) {
      msg <- "<b>The Null Hypothesis is TRUE (All means are equal).</b><br/> The bars represent the <b>Type I Error Rate</b>. They should sit at the dashed 5% line. If a test is significantly above 5%, it is falsely declaring significance due to violated assumptions."
    } else {
      msg <- "<b>The Null Hypothesis is FALSE (Means differ).</b><br/> The bars represent <b>Statistical Power</b>. Higher is better."
    }
    
    HTML(paste0("<div style='font-size: 16px; background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>", msg, "</div>"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)