# ==============================================================================
# INMR95: Type I Error (Alpha), Type II Error (Beta), & Power Simulator
# Welch's t-Test vs. Mann-Whitney U (Why Mann-Whitney is NOT a Safe Default!)
# Save as app.R and run via shiny::runApp()
# ==============================================================================
library(shiny)

ui <- fluidPage(
  titlePanel("INMR95: Alpha (\u03b1), Beta (\u03b2), & Power — Welch's t-Test vs. Mann-Whitney U"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Population Shapes (Equal Means at d = 0)"),
      selectInput("dist1", "Group 1 Distribution:",
                  choices = c("Log-Normal (Heavy Right Skew)" = "lnorm",
                              "Exponential (Moderate Right Skew)" = "exp",
                              "Heavy-Tailed Symmetric (Student t, df = 3)" = "t3",
                              "Uniform (Light / Bounded Tails)" = "unif",
                              "Normal (Symmetric Baseline)" = "norm"),
                  selected = "lnorm"),
      
      selectInput("dist2", "Group 2 Distribution (To Test Shape Asymmetry):",
                  choices = c("Same Shape as Group 1" = "same",
                              "Normal (Same Mean & SD, Different Shape!)" = "norm"),
                  selected = "same"),
      
      hr(),
      h4("2. Sample Size & Variance Ratio"),
      helpText("Tip: Increase SD Ratio > 1 on skewed data or set Group 2 = Normal to watch Mann-Whitney's Alpha (\u03b1) explode!"),
      sliderInput("n", "Sample Size per Group (n1 = n2):",
                  min = 8, max = 60, value = 25, step = 2),
      sliderInput("sd_ratio", "SD Ratio (SD2 / SD1):",
                  min = 1.0, max = 4.0, value = 1.0, step = 0.5),
      sliderInput("target_d", "Inspect Effect Size (Cohen's d for Power/\u03b2):",
                  min = 0.2, max = 1.4, value = 0.8, step = 0.2),
      
      hr(),
      h4("3. Display Options"),
      radioButtons("metric", "Y-Axis Metric for Plot 1:",
                   choices = c("Type II Error Rate (\u03b2 % - False Negatives)" = "beta",
                               "Statistical Power (1 - \u03b2 % - Rejection Rate)" = "power"),
                   selected = "power"),
      
      numericInput("sims", "Simulations per Effect Size:",
                   value = 1500, min = 500, max = 5000, step = 500),
      actionButton("resim", "Run New Monte Carlo Batch", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      # Top Summary Banner showing BOTH Alpha (at d = 0) and Beta/Power (at target d)
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 5px solid #2c3e50; padding: 12px;",
        h4(style = "margin-top: 0; color: #b22222;", textOutput("alpha_headline")),
        h4(style = "margin-top: 6px; color: #1b4f72;", textOutput("power_headline")),
        p(style = "margin-bottom: 0; font-size: 0.92em;", textOutput("diagnostic_subtext"))
      ),
      
      # 2x2 Plot Grid
      fluidRow(
        column(6, plotOutput("curve_plot", height = "320px")),
        column(6, plotOutput("alpha_power_barplot", height = "320px"))
      ),
      fluidRow(
        column(6, plotOutput("pop_h0_plot", height = "300px")),
        column(6, plotOutput("sd_bomb_plot", height = "300px"))
      )
    )
  )
)

server <- function(input, output) {
  
  # 1. Standardized Random Number Generator (True Mean = 0, True SD = 1)
  r_std <- function(total_n, dist_type) {
    if (dist_type == "norm") {
      return(rnorm(total_n, mean = 0, sd = 1))
    } else if (dist_type == "exp") {
      return(rexp(total_n, rate = 1) - 1)
    } else if (dist_type == "lnorm") {
      s <- 0.85
      raw <- rlnorm(total_n, meanlog = 0, sdlog = s)
      m <- exp(s^2 / 2)
      sd_val <- sqrt((exp(s^2) - 1) * exp(s^2))
      return((raw - m) / sd_val)
    } else if (dist_type == "t3") {
      return(rt(total_n, df = 3) / sqrt(3))
    } else if (dist_type == "unif") {
      return(runif(total_n, min = -sqrt(3), max = sqrt(3)))
    }
  }
  
  # 2. Fast Vectorized Mann-Whitney U (Wilcoxon Rank-Sum) P-values
  fast_wilcox_p <- function(mat1, mat2) {
    n1 <- ncol(mat1)
    n2 <- ncol(mat2)
    N <- n1 + n2
    combined <- cbind(mat1, mat2)
    
    ranks <- apply(combined, 1, rank)
    W1 <- colSums(ranks[1:n1, , drop = FALSE])
    U1 <- W1 - n1 * (n1 + 1) / 2
    
    mu_U <- (n1 * n2) / 2
    sigma_U <- sqrt(n1 * n2 * (N + 1) / 12)
    z <- (abs(U1 - mu_U) - 0.5) / sigma_U
    2 * pnorm(-abs(z))
  }
  
  # 3. Main Reactive Simulation Engine across Effect Sizes d = 0.0 to 1.4
  sim_engine <- reactive({
    input$resim
    
    n <- input$n
    B <- input$sims
    d1 <- input$dist1
    d2 <- ifelse(input$dist2 == "same", d1, input$dist2)
    sd_r <- input$sd_ratio
    
    # Pooled true SD so Cohen's d = (mu1 - mu2) / sigma_pooled remains properly scaled
    sigma_pool_true <- sqrt((1^2 + sd_r^2) / 2)
    d_seq <- seq(0.0, 1.4, by = 0.2)
    
    # Draw base matrices under H0 (both have True Mean = 0)
    base_mat1 <- matrix(r_std(B * n, d1) * 1.0,  nrow = B, ncol = n)
    base_mat2 <- matrix(r_std(B * n, d2) * sd_r, nrow = B, ncol = n)
    
    m1_base <- rowMeans(base_mat1)
    m2_base <- rowMeans(base_mat2)
    v1 <- apply(base_mat1, 1, var)
    v2 <- apply(base_mat2, 1, var)
    
    # Welch-Satterthwaite SE and degrees of freedom
    se_welch <- sqrt(v1 / n + v2 / n)
    df_welch <- (v1 / n + v2 / n)^2 / (((v1 / n)^2) / (n - 1) + ((v2 / n)^2) / (n - 1))
    s_pool_scaled <- sqrt((v1 + v2) / 2) / sigma_pool_true
    
    # Theoretical Normal Welch reference
    df_theo <- (1 + sd_r^2)^2 / ((1^2 + sd_r^4) / (n - 1))
    t_crit_theo <- qt(0.975, df = df_theo)
    
    power_theo <- numeric(length(d_seq))
    power_t_emp <- numeric(length(d_seq))
    power_mw_emp <- numeric(length(d_seq))
    
    for (i in seq_along(d_seq)) {
      d <- d_seq[i]
      shift <- d * sigma_pool_true # True mean shift in raw units
      
      # 1. Theoretical Normal Power (Non-central t)
      ncp <- d * sqrt(n / 2)
      power_theo[i] <- pt(-t_crit_theo, df = df_theo, ncp = ncp) +
        (1 - pt(t_crit_theo, df = df_theo, ncp = ncp))
      
      # 2. Empirical Welch t-test Rejection Rate
      t_stats <- (m1_base + shift - m2_base) / se_welch
      p_t <- 2 * pt(-abs(t_stats), df = df_welch)
      power_t_emp[i] <- mean(p_t < 0.05)
      
      # 3. Empirical Mann-Whitney U Rejection Rate
      p_mw <- fast_wilcox_p(base_mat1 + shift, base_mat2)
      power_mw_emp[i] <- mean(p_mw < 0.05)
    }
    
    target_idx <- which.min(abs(d_seq - input$target_d))
    
    list(
      d1 = d1,
      d2 = d2,
      sd_r = sd_r,
      d_seq = d_seq,
      target_idx = target_idx,
      # At d = 0.0 (index 1), the rejection rate IS the Type I Error (Alpha)!
      alpha_t = power_t_emp[1] * 100,
      alpha_mw = power_mw_emp[1] * 100,
      power_theo = power_theo * 100,
      power_t_emp = power_t_emp * 100,
      power_mw_emp = power_mw_emp * 100,
      beta_theo = (1 - power_theo) * 100,
      beta_t_emp = (1 - power_t_emp) * 100,
      beta_mw_emp = (1 - power_mw_emp) * 100,
      s_pool_scaled = s_pool_scaled
    )
  })
  
  # Top Headline 1: ALPHA (Type I Error at d = 0.0)
  output$alpha_headline <- renderText({
    res <- sim_engine()
    mw_warn <- ifelse(res$alpha_mw > 7.5, " \u26a0\ufe0f BROKEN (False Positives!)", " \u2705 Valid")
    t_warn  <- ifelse(res$alpha_t  > 7.5, " \u26a0\ufe0f Inflated", " \u2705 Robust")
    sprintf("1. Type I Error (\u03b1 at d = 0.0, Nominal = 5.0%%):   Welch t-Test \u03b1 = %.1f%%%s   |   Mann-Whitney \u03b1 = %.1f%%%s",
            res$alpha_t, t_warn, res$alpha_mw, mw_warn)
  })
  
  # Top Headline 2: POWER & BETA (at Target d > 0)
  output$power_headline <- renderText({
    res <- sim_engine()
    idx <- res$target_idx
    d_val <- res$d_seq[idx]
    sprintf("2. At Effect Size d = %.1f:   Welch t-Test Power = %.1f%% (\u03b2 = %.1f%%)   |   Mann-Whitney Power = %.1f%% (\u03b2 = %.1f%%)",
            d_val, res$power_t_emp[idx], res$beta_t_emp[idx], res$power_mw_emp[idx], res$beta_mw_emp[idx])
  })
  
  output$diagnostic_subtext <- renderText({
    res <- sim_engine()
    if (res$d1 != res$d2 || res$sd_r > 1.0) {
      "WHY MANN-WHITNEY FAILS HERE: Even though both groups have the EXACT SAME TRUE MEAN (\u03bc1 = \u03bc2 = 0) at d = 0, their shapes or variances differ! Because Mann-Whitney tests rank dominance P(X > Y) = 0.5 rather than equal means, unequal skew or variance causes Mann-Whitney's Type I error (\u03b1) to explode!"
    } else {
      "PURE LOCATION SHIFT (Identical Shapes & Equal Variances): Here at d = 0, both tests hold \u03b1 \u2248 5.0%. Under identical skewed/heavy-tailed distributions, Mann-Whitney achieves higher power because ranking neutralizes the outlier 'Variance Bombs' shown in Plot 4."
    }
  })
  
  # PLOT 1: Curves Across d = 0.0 (Alpha) to d = 1.4
  output$curve_plot <- renderPlot({
    res <- sim_engine()
    d_seq <- res$d_seq
    idx <- res$target_idx
    
    if (input$metric == "beta") {
      y_theo <- res$beta_theo
      y_t <- res$beta_t_emp
      y_mw <- res$beta_mw_emp
      ylab_str <- "Type II Error Rate (\u03b2 % - False Negatives)"
      main_str <- "1. \u03b2 Across Effect Sizes (Note: At d=0, 100-\u03b2 = \u03b1!)"
      leg_pos <- "topright"
    } else {
      y_theo <- res$power_theo
      y_t <- res$power_t_emp
      y_mw <- res$power_mw_emp
      ylab_str <- "Rejection Rate % (At d=0: \u03b1  |  At d>0: Power 1-\u03b2)"
      main_str <- "1. Rejection Rate: d = 0 is \u03b1 (5%), d > 0 is Power (1-\u03b2)"
      leg_pos <- "bottomright"
    }
    
    par(mar = c(4.2, 4.2, 3, 1))
    plot(d_seq, y_theo, type = "l", lty = 2, lwd = 2.5, col = "gray40",
         ylim = c(0, 100), xlab = "True Standardized Effect Size (Cohen's d; d = 0 is H0 True!)",
         ylab = ylab_str, main = main_str)
    grid(col = "gray88")
    
    # Reference line at 5% (or 95% for beta)
    ref_h <- ifelse(input$metric == "beta", 95, 5)
    abline(h = ref_h, col = "red", lty = 3, lwd = 1.5)
    
    lines(d_seq, y_t, type = "b", pch = 19, lwd = 3, col = "dodgerblue3")
    lines(d_seq, y_mw, type = "b", pch = 17, lwd = 3, col = "forestgreen")
    lines(d_seq, y_theo, type = "b", pch = 1, lty = 2, lwd = 2, col = "gray40")
    
    # Circle d = 0 (Alpha!)
    points(0, y_t[1], cex = 2.2, lwd = 2.5, col = "dodgerblue4")
    points(0, y_mw[1], cex = 2.2, lwd = 2.5, col = "darkgreen")
    
    abline(v = d_seq[idx], col = "firebrick", lty = 3, lwd = 2)
    
    legend(leg_pos,
           legend = c("Normal Theory Baseline",
                      "Welch's t-Test (Tests Means)",
                      "Mann-Whitney U (Tests Ranks)",
                      "Nominal 5% \u03b1 Threshold (at d=0)"),
           col = c("gray40", "dodgerblue3", "forestgreen", "red"),
           lty = c(2, 1, 1, 3), lwd = c(2, 3, 3, 1.5), pch = c(1, 19, 17, NA),
           cex = 0.82, bg = "white")
  })
  
  # PLOT 2: Direct Bar Chart Comparison of ALPHA (d = 0) vs. BETA / POWER (d = target_d)
  output$alpha_power_barplot <- renderPlot({
    res <- sim_engine()
    idx <- res$target_idx
    d_val <- res$d_seq[idx]
    
    # Matrix of [t-test, Mann-Whitney] x [Alpha at d=0, Beta at d, Power at d]
    mat_bars <- rbind(
      c(res$alpha_t,  res$beta_t_emp[idx],  res$power_t_emp[idx]),
      c(res$alpha_mw, res$beta_mw_emp[idx], res$power_mw_emp[idx])
    )
    colnames(mat_bars) <- c("Type I Error (\u03b1)\nat d = 0.0 (Target: 5%)",
                            sprintf("Type II Error (\u03b2)\nat d = %.1f (Lower=Better)", d_val),
                            sprintf("Power (1 - \u03b2)\nat d = %.1f (Higher=Better)", d_val))
    
    par(mar = c(4.2, 4.2, 3, 1))
    bp <- barplot(mat_bars, beside = TRUE,
                  col = c("dodgerblue3", "forestgreen"), border = "white",
                  ylim = c(0, 112), ylab = "Percentage (%)",
                  main = "2. Head-to-Head: Type I Error (\u03b1), Type II Error (\u03b2), & Power")
    
    # Nominal 5% line across the Alpha column
    segments(x0 = bp[1, 1] - 0.6, y0 = 5.0, x1 = bp[2, 1] + 0.6, y1 = 5.0,
             col = "red", lwd = 2.5, lty = 2)
    
    text(x = bp, y = mat_bars + 5,
         labels = sprintf("%.1f%%", mat_bars), font = 2, cex = 0.9)
    
    legend("topright",
           legend = c("Welch's t-Test", "Mann-Whitney U", "Nominal \u03b1 = 5.0%"),
           fill = c("dodgerblue3", "forestgreen", NA), border = NA,
           lty = c(NA, NA, 2), lwd = c(NA, NA, 2.5), col = c(NA, NA, "red"),
           cex = 0.85, bty = "n")
  })
  
  # PLOT 3: Parent Populations Under H0 (d = 0, Equal True Means = 0!)
  output$pop_h0_plot <- renderPlot({
    res <- sim_engine()
    pop1 <- r_std(20000, res$d1) * 1.0
    pop2 <- r_std(20000, res$d2) * res$sd_r
    
    # Calculate true empirical P(X1 > X2) under H0 to show why Mann-Whitney breaks!
    prob_gt <- mean(pop1 > pop2) * 100
    
    par(mar = c(4.2, 4, 3, 1))
    p1_clip <- pop1[pop1 > -4.5 & pop1 < 6.5]
    p2_clip <- pop2[pop2 > -4.5 & pop2 < 6.5]
    
    hist(p1_clip, breaks = 60, probability = TRUE,
         col = adjustcolor("dodgerblue3", 0.5), border = "white",
         main = sprintf("3. H0 Populations (Both Means = 0, but P(G1 > G2) = %.1f%%!)", prob_gt),
         xlab = "Value under H0 (d = 0.0)", xlim = c(-4, 6))
    hist(p2_clip, breaks = 60, probability = TRUE,
         col = adjustcolor("darkorange", 0.5), border = "white", add = TRUE)
    
    abline(v = 0, col = "black", lwd = 2.5, lty = 2)
    legend("topright",
           legend = c("Group 1 (Mean = 0)",
                      sprintf("Group 2 (Mean = 0, SD = %.1f)", res$sd_r),
                      "Shared True Mean = 0.0"),
           fill = c(adjustcolor("dodgerblue3", 0.5), adjustcolor("darkorange", 0.5), NA),
           border = NA, lty = c(NA, NA, 2), lwd = c(NA, NA, 2.5), col = c(NA, NA, "black"),
           cex = 0.82, bty = "n")
  })
  
  # PLOT 4: The "Variance Bomb" Mechanism (Standardized S_pooled)
  output$sd_bomb_plot <- renderPlot({
    res <- sim_engine()
    s_clipped <- res$s_pool_scaled[res$s_pool_scaled < 3.0]
    med_s <- median(res$s_pool_scaled)
    
    par(mar = c(4.2, 4.2, 3, 1))
    h <- hist(s_clipped, breaks = 45, plot = FALSE)
    bar_cols <- ifelse(h$mids < 1.0, adjustcolor("steelblue3", 0.7),
                       ifelse(h$mids > 1.35, adjustcolor("firebrick2", 0.8),
                              adjustcolor("gray70", 0.7)))
    
    plot(h, freq = FALSE, col = bar_cols, border = "white",
         main = "4. Sampling Dist of S_pooled / True \u03c3_pooled",
         xlab = "Ratio of Sample SD to True Population SD (Target = 1.0)", xlim = c(0.2, 2.6))
    
    abline(v = 1.0, col = "black", lwd = 2.5, lty = 2)
    abline(v = med_s, col = "darkblue", lwd = 2.5, lty = 1)
    
    legend("topright",
           legend = c("True \u03c3 Ratio = 1.00",
                      sprintf("Median Sample Ratio = %.2f", med_s),
                      "Ratio < 1.0 (Boosts t)",
                      "Ratio > 1.35 (Variance Bomb!)"),
           col = c("black", "darkblue", "steelblue3", "firebrick2"),
           lty = c(2, 1, NA, NA), lwd = c(2.5, 2.5, NA, NA),
           fill = c(NA, NA, adjustcolor("steelblue3", 0.7), adjustcolor("firebrick2", 0.8)),
           border = NA, cex = 0.82, bty = "n")
  })
}

shinyApp(ui = ui, server = server)