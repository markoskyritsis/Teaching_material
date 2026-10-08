# ==============================================================================
# INMR95: The Lukacs-Geary Theorem & Why the t-Test Assumes Normality
# Save as app.R and run via shiny::runApp()
# ==============================================================================
library(shiny)

ui <- fluidPage(
  titlePanel("The Lukacs-Geary Theorem: Why Does the t-Test Assume Normality?"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Test Configuration"),
      radioButtons("test_type", "Select t-Test Mode:",
                   choices = c("1-Sample t-Test (Full Lukacs-Geary Effect)" = "one",
                               "2-Sample Welch t-Test (See Symmetry Cancellation)" = "two"),
                   selected = "one"),
      
      hr(),
      h4("2. Population Shape(s)"),
      selectInput("dist1", "Population 1 Distribution:",
                  choices = c("Exponential (Right-Skewed: r > 0)" = "exp",
                              "Log-Normal (Extreme Right Skew: r >> 0)" = "lnorm",
                              "Normal (Symmetric: Truly Independent!)" = "norm",
                              "Uniform (Bounded: Parabolic Dependence!)" = "unif",
                              "Bimodal (Symmetric Two-Peak: Arch Dependence)" = "bimodal"),
                  selected = "exp"),
      
      conditionalPanel(
        condition = "input.test_type == 'two'",
        selectInput("dist2", "Population 2 Distribution:",
                    choices = c("Same as Population 1 (Skewness Cancels!)" = "same",
                                "Normal (Asymmetric Skewness -> Breaks!)" = "norm",
                                "Left-Skewed Exponential (Opposite Skew -> Breaks!)" = "neg_exp"),
                    selected = "same")
      ),
      
      hr(),
      h4("3. Sample Size(s)"),
      sliderInput("n1", "Sample Size (n1):", min = 5, max = 100, value = 10, step = 5),
      
      conditionalPanel(
        condition = "input.test_type == 'two'",
        sliderInput("n2", "Sample Size Group 2 (n2):", min = 5, max = 100, value = 10, step = 5),
        sliderInput("sd_ratio", "SD Ratio (SD2 / SD1):", min = 1, max = 4, value = 1, step = 0.5)
      ),
      
      hr(),
      numericInput("sims", "Monte Carlo Simulations:", value = 3000, min = 1000, max = 10000, step = 1000),
      actionButton("resim", "Draw New Samples", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      # Top Summary Banner
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 5px solid #2c3e50; padding: 12px;",
        h4(style = "margin-top: 0;", textOutput("headline_metrics")),
        p(style = "margin-bottom: 0; font-size: 0.95em;",
          HTML("<b>Lukacs-Geary Theorem (1942/1936):</b> The sample mean (<b>X&#772;</b>, numerator) and sample standard deviation (<b>S</b>, denominator) are statistically independent <i>if and only if</i> the parent population is Normal. Watch Plot 2 below: when <b>X&#772;</b> and <b>S</b> are correlated or parabolically linked, they warp the tails of the t-distribution!"))
      ),
      
      # 2x2 Diagnostic Plot Grid
      fluidRow(
        column(6, plotOutput("pop_plot", height = "310px")),
        column(6, plotOutput("lukacs_plot", height = "310px"))
      ),
      fluidRow(
        column(6, plotOutput("t_dist_plot", height = "320px")),
        column(6, plotOutput("tail_error_plot", height = "320px"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Helper to generate standardized distributions (True Mean = 0, True SD = 1)
  r_std_dist <- function(n, dist_type) {
    if (dist_type == "norm") {
      return(rnorm(n, mean = 0, sd = 1))
    } else if (dist_type == "exp") {
      # Exponential(1) shifted by -1 has mean = 0, SD = 1, skewness = +2
      return(rexp(n, rate = 1) - 1)
    } else if (dist_type == "neg_exp") {
      # Left-skewed Exponential: mean = 0, SD = 1, skewness = -2
      return(1 - rexp(n, rate = 1))
    } else if (dist_type == "lnorm") {
      # Log-Normal with sigma_log = 0.85 standardized to mean = 0, SD = 1
      s <- 0.85
      raw <- rlnorm(n, meanlog = 0, sdlog = s)
      m <- exp(s^2 / 2)
      sd_val <- sqrt((exp(s^2) - 1) * exp(s^2))
      return((raw - m) / sd_val)
    } else if (dist_type == "unif") {
      # Uniform(-sqrt(3), +sqrt(3)) has mean = 0, SD = 1
      return(runif(n, min = -sqrt(3), max = sqrt(3)))
    } else if (dist_type == "bimodal") {
      # Symmetric two-peak mixture standardized to mean = 0, SD = 1
      peaks <- sample(c(-1.3, 1.3), size = n, replace = TRUE)
      raw <- rnorm(n, mean = peaks, sd = 0.4)
      return(raw / sqrt(1.3^2 + 0.4^2))
    }
  }
  
  sim_results <- reactive({
    input$resim
    
    B <- input$sims
    n1 <- input$n1
    d1 <- input$dist1
    
    # Simulate Group 1
    mat1 <- matrix(r_std_dist(n1 * B, d1), nrow = B, ncol = n1)
    m1 <- rowMeans(mat1)
    s1 <- apply(mat1, 1, sd)
    
    if (input$test_type == "one") {
      # 1-Sample t-test (True mu = 0)
      num <- m1
      se <- s1 / sqrt(n1)
      df <- rep(n1 - 1, B)
      t_stats <- num / se
      t_crit <- qt(0.975, df = n1 - 1)
      
      x_scatter <- m1
      y_scatter <- s1
      scatter_xlab <- "Sample Mean (Xbar) [Numerator]"
      scatter_ylab <- "Sample SD (S) [Denominator]"
      df_theo <- n1 - 1
    } else {
      # 2-Sample Welch t-test (True mu1 = mu2 = 0)
      n2 <- input$n2
      d2 <- ifelse(input$dist2 == "same", d1, input$dist2)
      sd_r <- input$sd_ratio
      
      mat2 <- matrix(r_std_dist(n2 * B, d2) * sd_r, nrow = B, ncol = n2)
      m2 <- rowMeans(mat2)
      s2 <- apply(mat2, 1, sd)
      
      num <- m1 - m2
      v1_n <- (s1^2) / n1
      v2_n <- (s2^2) / n2
      se <- sqrt(v1_n + v2_n)
      df <- (v1_n + v2_n)^2 / ((v1_n^2)/(n1 - 1) + (v2_n^2)/(n2 - 1))
      t_stats <- num / se
      t_crit <- qt(0.975, df = df)
      
      x_scatter <- num
      y_scatter <- se
      scatter_xlab <- "Difference in Means (Xbar1 - Xbar2) [Numerator]"
      scatter_ylab <- "Standard Error SE [Denominator]"
      df_theo <- mean(df)
    }
    
    left_rej <- t_stats < -t_crit
    right_rej <- t_stats > t_crit
    
    # Linear correlation & Quadratic R-squared (to detect Uniform/Bimodal arches!)
    r_lin <- cor(x_scatter, y_scatter)
    quad_fit <- lm(y_scatter ~ x_scatter + I(x_scatter^2))
    r2_quad <- summary(quad_fit)$r.squared
    
    list(
      d1 = d1,
      d2 = ifelse(input$test_type == "two", ifelse(input$dist2 == "same", d1, input$dist2), NA),
      x_scatter = x_scatter,
      y_scatter = y_scatter,
      scatter_xlab = scatter_xlab,
      scatter_ylab = scatter_ylab,
      t_stats = t_stats,
      df_theo = df_theo,
      left_rej = left_rej,
      right_rej = right_rej,
      r_lin = r_lin,
      r2_quad = r2_quad,
      quad_coefs = coef(quad_fit)
    )
  })
  
  output$headline_metrics <- renderText({
    res <- sim_results()
    tot_alpha <- mean(res$left_rej | res$right_rej) * 100
    left_alpha <- mean(res$left_rej) * 100
    right_alpha <- mean(res$right_rej) * 100
    
    sprintf(
      "Linear Cor(Num, Denom) = %+.2f  |  Quadratic R² = %.2f  ||  Total Type I Error = %.2f%% (Left Tail: %.2f%%, Right Tail: %.2f%%)",
      res$r_lin, res$r2_quad, tot_alpha, left_alpha, right_alpha
    )
  })
  
  # PLOT 1: Parent Population Shape
  output$pop_plot <- renderPlot({
    res <- sim_results()
    pop1 <- r_std_dist(15000, res$d1)
    
    par(mar = c(4.2, 4, 3, 1))
    if (input$test_type == "one" || res$d1 == res$d2) {
      hist(pop1, breaks = 60, probability = TRUE, col = adjustcolor("steelblue", 0.7),
           border = "white", main = "1. True Parent Population (Mean = 0)",
           xlab = "Value (X)", xlim = c(-3.5, 5.5))
      abline(v = 0, col = "black", lwd = 2, lty = 2)
    } else {
      pop2 <- r_std_dist(15000, res$d2) * input$sd_ratio
      hist(pop1, breaks = 60, probability = TRUE, col = adjustcolor("steelblue", 0.5),
           border = "white", main = "1. Parent Populations (Both True Means = 0)",
           xlab = "Value (X)", xlim = c(-4.5, 5.5))
      hist(pop2, breaks = 60, probability = TRUE, col = adjustcolor("darkorange", 0.5),
           border = "white", add = TRUE)
      abline(v = 0, col = "black", lwd = 2, lty = 2)
      legend("topright", legend = c("Group 1", "Group 2"),
             fill = c(adjustcolor("steelblue", 0.6), adjustcolor("darkorange", 0.6)), bty = "n")
    }
  })
  
  # PLOT 2: Lukacs-Geary Joint Distribution (Numerator vs Denominator)
  output$lukacs_plot <- renderPlot({
    res <- sim_results()
    x <- res$x_scatter
    y <- res$y_scatter
    
    # Color points by whether they caused a Type I error in the Left or Right tail!
    pt_cols <- ifelse(res$left_rej, adjustcolor("dodgerblue3", 0.8),
                      ifelse(res$right_rej, adjustcolor("firebrick2", 0.8),
                             adjustcolor("gray55", 0.35)))
    pt_pch <- ifelse(res$left_rej | res$right_rej, 19, 16)
    pt_cex <- ifelse(res$left_rej | res$right_rej, 0.9, 0.65)
    
    par(mar = c(4.2, 4.2, 3, 1))
    plot(x, y, col = pt_cols, pch = pt_pch, cex = pt_cex,
         main = sprintf("2. Lukacs-Geary Plot: Numerator vs. Denominator (r = %+.2f)", res$r_lin),
         xlab = res$scatter_xlab, ylab = res$scatter_ylab)
    
    # Add quadratic trend curve to show both linear slope (skew) and parabolic arch (bounded)
    x_seq <- seq(min(x), max(x), length.out = 200)
    cc <- res$quad_coefs
    y_pred <- cc[1] + cc[2] * x_seq + cc[3] * (x_seq^2)
    lines(x_seq, y_pred, col = "darkgreen", lwd = 3)
    
    legend("topright",
           legend = c("Non-Significant (|t| < t_crit)",
                      "Left-Tail Reject (Small S explodes -t!)",
                      "Right-Tail Reject (Large S shrinks +t!)",
                      "Conditional Mean Trend"),
           col = c("gray55", "dodgerblue3", "firebrick2", "darkgreen"),
           pch = c(16, 19, 19, NA), lty = c(NA, NA, NA, 1), lwd = c(NA, NA, NA, 3),
           cex = 0.82, bty = "n")
  })
  
  # PLOT 3: Simulated t-Statistics vs. Theoretical Student's t
  output$t_dist_plot <- renderPlot({
    res <- sim_results()
    t_clipped <- res$t_stats[res$t_stats > -7 & res$t_stats < 7]
    t_crit <- qt(0.975, df = res$df_theo)
    
    par(mar = c(4.2, 4, 3, 1))
    hist(t_clipped, breaks = 50, probability = TRUE, col = "slategray3", border = "white",
         main = "3. Simulated t-Statistic vs. Theoretical Student's t",
         xlab = "t-statistic (clipped to [-7, 7] for display)", xlim = c(-6, 6), ylim = c(0, 0.45))
    
    curve(dt(x, df = res$df_theo), add = TRUE, col = "darkred", lwd = 2.5)
    abline(v = c(-t_crit, t_crit), col = c("dodgerblue3", "firebrick2"), lwd = 2, lty = 2)
    
    legend("topright",
           legend = c("Simulated t", "Theoretical t", "Left Crit (-t)", "Right Crit (+t)"),
           fill = c("slategray3", NA, NA, NA), border = c("white", NA, NA, NA),
           lty = c(NA, 1, 2, 2), lwd = c(NA, 2.5, 2, 2),
           col = c(NA, "darkred", "dodgerblue3", "firebrick2"), cex = 0.85, bty = "n")
  })
  
  # PLOT 4: Tail-by-Tail Type I Error Breakdown
  output$tail_error_plot <- renderPlot({
    res <- sim_results()
    left_err <- mean(res$left_rej) * 100
    right_err <- mean(res$right_rej) * 100
    tot_err <- left_err + right_err
    
    rates <- c(left_err, right_err, tot_err)
    names_bar <- c("Left Tail\n(Nominal = 2.5%)", "Right Tail\n(Nominal = 2.5%)", "Two-Tailed Total\n(Nominal = 5.0%)")
    cols_bar <- c("dodgerblue3", "firebrick2", "darkslategray")
    
    par(mar = c(4.2, 4.2, 3, 1))
    y_max <- max(12, max(rates) * 1.2)
    bp <- barplot(rates, names.arg = names_bar, col = cols_bar, border = "white",
                  ylim = c(0, y_max), ylab = "Empirical Rejection Rate (%)",
                  main = "4. Empirical Type I Error Breakdown by Tail")
    
    # Nominal reference lines
    segments(x0 = bp[1] - 0.5, y0 = 2.5, x1 = bp[2] + 0.5, y1 = 2.5, col = "black", lwd = 2, lty = 2)
    segments(x0 = bp[3] - 0.5, y0 = 5.0, x1 = bp[3] + 0.5, y1 = 5.0, col = "black", lwd = 2, lty = 2)
    
    text(x = bp, y = rates + y_max * 0.05,
         labels = sprintf("%.2f%%", rates), font = 2, cex = 1.05)
    
    legend("topleft", legend = "Nominal Alpha Target (2.5% per tail / 5.0% total)",
           lty = 2, lwd = 2, col = "black", cex = 0.85, bty = "n")
  })
}

shinyApp(ui = ui, server = server)