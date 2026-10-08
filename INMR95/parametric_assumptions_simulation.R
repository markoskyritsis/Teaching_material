# Save as app.R and run via shiny::runApp()
library(shiny)

ui <- fluidPage(
  titlePanel("INMR95: Try to Break the Two-Sample t-Test!"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Population Shape (Normality)"),
      selectInput("dist", "Parent Distribution (Both have equal means):",
                  choices = c("Normal (Symmetric)" = "norm",
                              "Exponential (Heavy Right Skew)" = "exp",
                              "Uniform (Flat / Light Tails)" = "unif",
                              "Bimodal (Two Peaks)" = "bimodal")),
      
      hr(),
      h4("2. Sample Sizes & Variances"),
      helpText("Tip: Try unequal sample sizes paired with unequal SDs!"),
      sliderInput("n1", "Sample Size Group 1 (n1):", min = 5, max = 100, value = 15, step = 5),
      sliderInput("n2", "Sample Size Group 2 (n2):", min = 5, max = 100, value = 15, step = 5),
      sliderInput("sd_ratio", "SD Ratio (SD2 / SD1):", min = 1, max = 5, value = 1, step = 0.5),
      
      hr(),
      h4("3. Test Type"),
      radioButtons("var_equal", "Variance Assumption:",
                   choices = c("Student's t-test (Assume Equal Variances)" = "TRUE",
                               "Welch's t-test (Do Not Assume Equal Variances - R Default)" = "FALSE"),
                   selected = "TRUE"),
      
      numericInput("sims", "Number of Simulations:", value = 2500, min = 500, max = 10000, step = 500),
      actionButton("resim", "Run New Simulation Batch", class = "btn-primary")
    ),
    
    mainPanel(
      fluidRow(
        column(12,
               wellPanel(
                 h4(textOutput("type1_headline")),
                 p("Under a true Null Hypothesis (no difference in population means), a valid test at alpha = 0.05 should reject H0 about 5% of the time. If the Empirical Type I Error rate below strays far from 5%, the test's assumptions have failed!")
               )
        )
      ),
      fluidRow(
        column(6, plotOutput("pop_plot", height = "280px")),
        column(6, plotOutput("mean_diff_plot", height = "280px"))
      ),
      fluidRow(
        column(6, plotOutput("t_dist_plot", height = "300px")),
        column(6, plotOutput("p_val_plot", height = "300px"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Helper function to generate standardized random variables (mean = 0, SD = 1)
  r_standardized <- function(n, dist_type) {
    if (dist_type == "norm") {
      return(rnorm(n, mean = 0, sd = 1))
    } else if (dist_type == "exp") {
      # rexp(rate=1) has mean = 1, sd = 1 -> subtract 1 to center at 0
      return(rexp(n, rate = 1) - 1)
    } else if (dist_type == "unif") {
      # runif(-sqrt(3), sqrt(3)) has mean = 0, sd = 1
      return(runif(n, min = -sqrt(3), max = sqrt(3)))
    } else if (dist_type == "bimodal") {
      # Mixture of two normals at -1.2 and +1.2, scaled to SD = 1
      peaks <- sample(c(-1.2, 1.2), size = n, replace = TRUE)
      raw <- rnorm(n, mean = peaks, sd = 0.5)
      return(raw / sqrt(1.2^2 + 0.5^2))
    }
  }
  
  sim_data <- reactive({
    input$resim # Trigger on button click or input change
    
    n1 <- input$n1
    n2 <- input$n2
    sd1 <- 1
    sd2 <- input$sd_ratio
    B <- input$sims
    pool_var <- as.logical(input$var_equal)
    
    # Matrix generation for fast simulation
    mat1 <- matrix(r_standardized(n1 * B, input$dist) * sd1, nrow = B, ncol = n1)
    mat2 <- matrix(r_standardized(n2 * B, input$dist) * sd2, nrow = B, ncol = n2)
    
    m1 <- rowMeans(mat1)
    m2 <- rowMeans(mat2)
    v1 <- apply(mat1, 1, var)
    v2 <- apply(mat2, 1, var)
    
    mean_diffs <- m1 - m2
    
    if (pool_var) {
      # Student's Pooled t-test
      df <- n1 + n2 - 2
      s_pool <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / df)
      se <- s_pool * sqrt(1/n1 + 1/n2)
      t_stats <- mean_diffs / se
      p_vals <- 2 * pt(-abs(t_stats), df = df)
    } else {
      # Welch-Satterthwaite t-test
      se <- sqrt(v1/n1 + v2/n2)
      t_stats <- mean_diffs / se
      df <- (v1/n1 + v2/n2)^2 / ((v1/n1)^2/(n1 - 1) + (v2/n2)^2/(n2 - 1))
      p_vals <- 2 * pt(-abs(t_stats), df = df)
    }
    
    list(
      pop1 = mat1[1, ],
      pop2 = mat2[1, ],
      mean_diffs = mean_diffs,
      t_stats = t_stats,
      p_vals = p_vals,
      df_approx = n1 + n2 - 2
    )
  })
  
  output$type1_headline <- renderText({
    p_vals <- sim_data()$p_vals
    err_rate <- mean(p_vals < 0.05) * 100
    status <- ifelse(err_rate > 6.5, "⚠️ INFLATED (Too many false positives!)",
                     ifelse(err_rate < 3.5, "⚠️ CONSERVATIVE (Losing statistical power!)",
                            "✅ ROBUST (Close to nominal 5% rate)"))
    sprintf("Empirical Type I Error Rate (alpha = 5%%): %.2f%% — %s", err_rate, status)
  })
  
  output$pop_plot <- renderPlot({
    # Generate a large sample just to show the true underlying population shape
    pop_sample <- r_standardized(10000, input$dist)
    hist(pop_sample, breaks = 50, probability = TRUE, col = "steelblue", border = "white",
         main = "1. True Parent Population Shape (Group 1)",
         xlab = "Value (Mean = 0, SD = 1)", xlim = c(-4, 5))
  })
  
  output$mean_diff_plot <- renderPlot({
    diffs <- sim_data()$mean_diffs
    hist(diffs, breaks = 40, probability = TRUE, col = "mediumseagreen", border = "white",
         main = "2. Sampling Dist of (Xbar1 - Xbar2) [CLT]",
         xlab = "Difference in Sample Means")
    curve(dnorm(x, mean = mean(diffs), sd = sd(diffs)), add = TRUE, col = "darkgreen", lwd = 2)
  })
  
  output$t_dist_plot <- renderPlot({
    res <- sim_data()
    hist(res$t_stats, breaks = 40, probability = TRUE, col = "slategray3", border = "white",
         main = "3. Simulated t-Statistics vs. Theoretical t",
         xlab = "t-statistic", xlim = c(-5, 5))
    curve(dt(x, df = res$df_approx), add = TRUE, col = "darkred", lwd = 2)
    legend("topright", legend = c("Simulated", "Theoretical t"),
           fill = c("slategray3", NA), border = c("white", NA),
           lty = c(NA, 1), lwd = c(NA, 2), col = c(NA, "darkred"), bty = "n")
  })
  
  output$p_val_plot <- renderPlot({
    p_vals <- sim_data()$p_vals
    # 20 breaks means each bar is 0.05 wide!
    h <- hist(p_vals, breaks = seq(0, 1, by = 0.05), plot = FALSE)
    bar_cols <- ifelse(h$mids < 0.05, "firebrick", "lightgray")
    plot(h, freq = FALSE, col = bar_cols, border = "white",
         main = "4. P-Value Distribution (First Bar = P < 0.05)",
         xlab = "p-value (Each bar = 5% width)", ylab = "Density")
    abline(h = 1, col = "red", lwd = 2, lty = 2)
    legend("topright", legend = c("Alpha < 0.05 Bin", "Expected Uniform (5% per bin)"),
           fill = c("firebrick", NA), border = c("white", NA),
           lty = c(NA, 2), lwd = c(NA, 2), col = c(NA, "red"), bty = "n")
  })
}

shinyApp(ui = ui, server = server)