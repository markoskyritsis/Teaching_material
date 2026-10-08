# ==============================================================================
# INMR95: The Behrens-Fisher Problem Explorer
# Student's Pooled t-Test vs. Welch-Satterthwaite t-Test Under Unequal Variances
# Save as app.R and run via shiny::runApp()
# ==============================================================================
library(shiny)

ui <- fluidPage(
  titlePanel("INMR95: The Behrens-Fisher Problem — Equal vs. Unequal Sample Sizes & Variances"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Quick Classroom Presets"),
      helpText("Click a preset below to jump straight to the three key scenarios:"),
      actionButton("preset_equal_n", "1. Equal n, Unequal SD (n1=30, n2=30)",
                   class = "btn-info btn-block", style = "margin-bottom: 6px;"),
      actionButton("preset_small_n_big_sd", "2. Small n has Big SD (n1=50, n2=10)",
                   class = "btn-danger btn-block", style = "margin-bottom: 6px;"),
      actionButton("preset_big_n_big_sd", "3. Big n has Big SD (n1=10, n2=50)",
                   class = "btn-warning btn-block", style = "margin-bottom: 12px;"),
      
      hr(),
      h4("2. Sample Sizes & Standard Deviations"),
      sliderInput("n1", "Group 1 Sample Size (n1):", min = 5, max = 100, value = 30, step = 5),
      sliderInput("n2", "Group 2 Sample Size (n2):", min = 5, max = 100, value = 30, step = 5),
      sliderInput("sd1", "Group 1 True SD (\u03c31):", min = 1.0, max = 5.0, value = 1.0, step = 0.5),
      sliderInput("sd2", "Group 2 True SD (\u03c32):", min = 1.0, max = 5.0, value = 4.0, step = 0.5),
      
      hr(),
      h4("3. True Mean Difference (For Power)"),
      sliderInput("delta", "True Mean Shift (\u03bc1 - \u03bc2) for Plot 4:",
                  min = 0.5, max = 3.0, value = 1.5, step = 0.25),
      numericInput("sims", "Monte Carlo Simulations:", value = 4000, min = 1000, max = 10000, step = 1000),
      actionButton("resim", "Run New Simulation Batch", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      # Top Diagnostic Banner
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 5px solid #2c3e50; padding: 12px;",
        h4(style = "margin-top: 0; color: #b22222;", textOutput("headline_alpha")),
        h4(style = "margin-top: 6px; color: #1b4f72;", textOutput("headline_se")),
        p(style = "margin-bottom: 0; font-size: 0.93em;", textOutput("explanation_text"))
      ),
      
      # 2x2 Diagnostic Grid
      fluidRow(
        column(6, plotOutput("se_plot", height = "310px")),
        column(6, plotOutput("t_dist_plot", height = "310px"))
      ),
      fluidRow(
        column(6, plotOutput("df_plot", height = "310px")),
        column(6, plotOutput("alpha_power_plot", height = "310px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Preset 1: Equal n (30 vs 30), Unequal SD (1 vs 4) -> Student's t is SAFE!
  observeEvent(input$preset_equal_n, {
    updateSliderInput(session, "n1", value = 30)
    updateSliderInput(session, "n2", value = 30)
    updateSliderInput(session, "sd1", value = 1.0)
    updateSliderInput(session, "sd2", value = 4.0)
  })
  
  # Preset 2: Smaller n has Larger SD (n1=50, SD1=1 vs n2=10, SD2=4) -> Alpha EXPLODES!
  observeEvent(input$preset_small_n_big_sd, {
    updateSliderInput(session, "n1", value = 50)
    updateSliderInput(session, "n2", value = 10)
    updateSliderInput(session, "sd1", value = 1.0)
    updateSliderInput(session, "sd2", value = 4.0)
  })
  
  # Preset 3: Larger n has Larger SD (n1=10, SD1=1 vs n2=50, SD2=4) -> Ultra-Conservative!
  observeEvent(input$preset_big_n_big_sd, {
    updateSliderInput(session, "n1", value = 10)
    updateSliderInput(session, "n2", value = 50)
    updateSliderInput(session, "sd1", value = 1.0)
    updateSliderInput(session, "sd2", value = 4.0)
  })
  
  sim_data <- reactive({
    input$resim
    
    n1 <- input$n1
    n2 <- input$n2
    sd1 <- input$sd1
    sd2 <- input$sd2
    delta <- input$delta
    B <- input$sims
    
    # True Standard Error of (Xbar1 - Xbar2)
    true_se <- sqrt((sd1^2) / n1 + (sd2^2) / n2)
    
    # Simulate B experiments under H0 (mu1 = mu2 = 0)
    mat1 <- matrix(rnorm(B * n1, mean = 0, sd = sd1), nrow = B, ncol = n1)
    mat2 <- matrix(rnorm(B * n2, mean = 0, sd = sd2), nrow = B, ncol = n2)
    
    m1 <- rowMeans(mat1)
    m2 <- rowMeans(mat2)
    v1 <- apply(mat1, 1, var)
    v2 <- apply(mat2, 1, var)
    
    diff_h0 <- m1 - m2
    diff_h1 <- (m1 + delta) - m2
    
    # 1. Student's Pooled t-testCalculations
    df_pool <- n1 + n2 - 2
    s2_pool <- ((n1 - 1) * v1 + (n2 - 1) * v2) / df_pool
    se_pool <- sqrt(s2_pool * (1 / n1 + 1 / n2))
    
    t_pool_h0 <- diff_h0 / se_pool
    p_pool_h0 <- 2 * pt(-abs(t_pool_h0), df = df_pool)
    
    t_pool_h1 <- diff_h1 / se_pool
    p_pool_h1 <- 2 * pt(-abs(t_pool_h1), df = df_pool)
    
    # 2. Welch-Satterthwaite t-test Calculations
    se_welch <- sqrt(v1 / n1 + v2 / n2)
    df_welch <- (v1 / n1 + v2 / n2)^2 / (((v1 / n1)^2) / (n1 - 1) + ((v2 / n2)^2) / (n2 - 1))
    
    t_welch_h0 <- diff_h0 / se_welch
    p_welch_h0 <- 2 * pt(-abs(t_welch_h0), df = df_welch)
    
    t_welch_h1 <- diff_h1 / se_welch
    p_welch_h1 <- 2 * pt(-abs(t_welch_h1), df = df_welch)
    
    list(
      n1 = n1, n2 = n2, sd1 = sd1, sd2 = sd2, delta = delta,
      true_se = true_se,
      se_pool = se_pool,
      se_welch = se_welch,
      df_pool = df_pool,
      df_welch = df_welch,
      t_pool_h0 = t_pool_h0,
      t_welch_h0 = t_welch_h0,
      alpha_pool = mean(p_pool_h0 < 0.05) * 100,
      alpha_welch = mean(p_welch_h0 < 0.05) * 100,
      power_pool = mean(p_pool_h1 < 0.05) * 100,
      power_welch = mean(p_welch_h1 < 0.05) * 100
    )
  })
  
  output$headline_alpha <- renderText({
    res <- sim_data()
    pool_status <- ifelse(res$alpha_pool > 6.5, " \u26a0\ufe0f INFLATED (False Positives!)",
                          ifelse(res$alpha_pool < 3.5, " \u26a0\ufe0f CONSERVATIVE (Kills Power!)",
                                 " \u2705 ROBUST"))
    sprintf("Empirical Type I Error (\u03b1, Nominal = 5.0%%):   Student's Pooled t = %.1f%%%s   |   Welch's t = %.1f%% \u2705",
            res$alpha_pool, pool_status, res$alpha_welch)
  })
  
  output$headline_se <- renderText({
    res <- sim_data()
    sprintf("Denominator Check:   True SE = %.3f   |   Mean Pooled SE = %.3f   |   Mean Welch SE = %.3f",
            res$true_se, mean(res$se_pool), mean(res$se_welch))
  })
  
  output$explanation_text <- renderText({
    res <- sim_data()
    if (res$n1 == res$n2) {
      "EQUAL SAMPLE SIZES (n1 = n2): Even when variances are wildly unequal, S_pooled * sqrt(1/n + 1/n) simplifies algebraically to sqrt(S1\u00b2/n + S2\u00b2/n)! Look at Plot 1: Pooled SE and Welch SE are 100% identical! The only difference is degrees of freedom (Plot 3)."
    } else if ((res$n1 < res$n2 && res$sd1 > res$sd2) || (res$n2 < res$n1 && res$sd2 > res$sd1)) {
      "SMALLER SAMPLE HAS LARGER VARIANCE: The larger sample dominates the weighted average in S_pooled, pulling Pooled SE far BELOW the True SE (Plot 1). Dividing by a tiny Pooled SE explodes Student's t-statistic (Plot 2), causing massive Type I error inflation!"
    } else if ((res$n1 > res$n2 && res$sd1 > res$sd2) || (res$n2 > res$n1 && res$sd2 > res$sd1)) {
      "LARGER SAMPLE HAS LARGER VARIANCE: The larger sample dominates S_pooled, pulling Pooled SE far ABOVE the True SE (Plot 1). Dividing by a bloated Pooled SE shrinks Student's t-statistic toward zero (Plot 2), dropping \u03b1 near 0% and destroying Statistical Power (Plot 4)!"
    } else {
      "EQUAL VARIANCES (\u03c31 = \u03c32): Both tests estimate the exact same True SE and maintain nominal 5% Type I error."
    }
  })
  
  # PLOT 1: The Denominator (Pooled SE vs. Welch SE vs. True SE)
  output$se_plot <- renderPlot({
    res <- sim_data()
    x_min <- min(c(res$se_pool, res$se_welch, res$true_se)) * 0.7
    x_max <- max(c(res$se_pool, res$se_welch, res$true_se)) * 1.3
    
    par(mar = c(4.2, 4, 3, 1))
    hist(res$se_welch, breaks = 40, probability = TRUE,
         col = adjustcolor("forestgreen", 0.55), border = "white",
         xlim = c(x_min, x_max),
         main = "1. The Denominator: Pooled SE vs. Welch SE",
         xlab = "Estimated Standard Error (SE) of (Xbar1 - Xbar2)")
    
    hist(res$se_pool, breaks = 40, probability = TRUE,
         col = adjustcolor("firebrick2", 0.55), border = "white", add = TRUE)
    
    abline(v = res$true_se, col = "black", lwd = 3, lty = 2)
    
    legend("topright",
           legend = c(sprintf("True SE = %.2f", res$true_se),
                      sprintf("Welch SE (Mean = %.2f)", mean(res$se_welch)),
                      sprintf("Pooled SE (Mean = %.2f)", mean(res$se_pool))),
           fill = c(NA, adjustcolor("forestgreen", 0.6), adjustcolor("firebrick2", 0.6)),
           border = NA, lty = c(2, NA, NA), lwd = c(3, NA, NA), col = c("black", NA, NA),
           cex = 0.85, bty = "n")
  })
  
  # PLOT 2: Simulated t-Distributions Under H0 vs. Theoretical t
  output$t_dist_plot <- renderPlot({
    res <- sim_data()
    t_pool_clip <- res$t_pool_h0[abs(res$t_pool_h0) < 6.5]
    t_welch_clip <- res$t_welch_h0[abs(res$t_welch_h0) < 6.5]
    
    par(mar = c(4.2, 4, 3, 1))
    hist(t_pool_clip, breaks = 50, probability = TRUE,
         col = adjustcolor("firebrick2", 0.5), border = "white",
         xlim = c(-5.5, 5.5), ylim = c(0, 0.50),
         main = "2. Resulting t-Statistics Under H0",
         xlab = "t-statistic (Dashed lines = Nominal \u00b1t_crit)")
    
    hist(t_welch_clip, breaks = 50, probability = TRUE,
         col = adjustcolor("forestgreen", 0.5), border = "white", add = TRUE)
    
    curve(dt(x, df = mean(res$df_welch)), add = TRUE, col = "black", lwd = 2.5)
    t_crit <- qt(0.975, df = res$df_pool)
    abline(v = c(-t_crit, t_crit), col = "darkred", lwd = 2, lty = 2)
    
    legend("topright",
           legend = c("Student's Pooled t", "Welch's t", "Theoretical t Curve", "\u00b1t_crit (5% Tails)"),
           fill = c(adjustcolor("firebrick2", 0.5), adjustcolor("forestgreen", 0.5), NA, NA),
           border = NA, lty = c(NA, NA, 1, 2), lwd = c(NA, NA, 2.5, 2),
           col = c(NA, NA, "black", "darkred"), cex = 0.82, bty = "n")
  })
  
  # PLOT 3: Satterthwaite Degrees of Freedom Penalty vs. Pooled df
  output$df_plot <- renderPlot({
    res <- sim_data()
    min_df <- min(res$n1 - 1, res$n2 - 1)
    max_df <- res$df_pool
    
    par(mar = c(4.2, 4, 3, 1))
    hist(res$df_welch, breaks = 35, probability = TRUE,
         col = adjustcolor("steelblue", 0.7), border = "white",
         xlim = c(max(1, min_df - 2), max_df + 5),
         main = "3. Degrees of Freedom: Welch Satterthwaite vs. Pooled",
         xlab = "Degrees of Freedom (df)")
    
    abline(v = max_df, col = "firebrick2", lwd = 3, lty = 2)
    abline(v = min_df, col = "darkorange3", lwd = 2.5, lty = 3)
    
    legend("topleft",
           legend = c(sprintf("Pooled df (n1+n2-2) = %d", max_df),
                      sprintf("Welch df (Mean = %.1f)", mean(res$df_welch)),
                      sprintf("Min Possible df (min(n)-1) = %d", min_df)),
           col = c("firebrick2", "steelblue", "darkorange3"),
           lty = c(2, 1, 3), lwd = c(3, 6, 2.5), cex = 0.82, bty = "n")
  })
  
  # PLOT 4: Type I Error (Alpha) and Statistical Power (1 - Beta) Side-by-Side
  output$alpha_power_plot <- renderPlot({
    res <- sim_data()
    mat_comp <- rbind(
      c(res$alpha_pool,  res$power_pool,  100 - res$power_pool),
      c(res$alpha_welch, res$power_welch, 100 - res$power_welch)
    )
    colnames(mat_comp) <- c("Type I Error (\u03b1)\n(H0 True, Target = 5%)",
                            sprintf("Power (1 - \u03b2)\n(at \u0394 = %.2f)", res$delta),
                            sprintf("Type II Error (\u03b2)\n(at \u0394 = %.2f)", res$delta))
    
    par(mar = c(4.2, 4.2, 3, 1))
    bp <- barplot(mat_comp, beside = TRUE,
                  col = c("firebrick2", "forestgreen"), border = "white",
                  ylim = c(0, 112), ylab = "Percentage (%)",
                  main = "4. Impact on Type I Error (\u03b1), Power (1-\u03b2), & Type II Error (\u03b2)")
    
    # Reference line at 5% for Alpha
    segments(x0 = bp[1, 1] - 0.6, y0 = 5.0, x1 = bp[2, 1] + 0.6, y1 = 5.0,
             col = "black", lwd = 2.5, lty = 2)
    
    text(x = bp, y = mat_comp + 5,
         labels = sprintf("%.1f%%", mat_comp), font = 2, cex = 0.9)
    
    legend("topright",
           legend = c("Student's Pooled t-Test", "Welch's t-Test (R Default)", "Nominal \u03b1 = 5.0%"),
           fill = c("firebrick2", "forestgreen", NA), border = NA,
           lty = c(NA, NA, 2), lwd = c(NA, NA, 2.5), col = c(NA, NA, "black"),
           cex = 0.82, bty = "n")
  })
}

shinyApp(ui = ui, server = server)