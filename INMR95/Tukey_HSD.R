library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("The Post-Hoc Penalty: Tukey vs. Bonferroni"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Group Means"),
      sliderInput("mean_A", "Group A (Control):", min = 40, max = 60, value = 50, step = 1),
      sliderInput("mean_B", "Group B:", min = 40, max = 60, value = 50, step = 1),
      sliderInput("mean_C", "Group C:", min = 40, max = 60, value = 55, step = 1),
      sliderInput("mean_D", "Group D:", min = 40, max = 60, value = 58, step = 1),
      
      hr(),
      h4("Shared Parameters"),
      sliderInput("sd", "Standard Deviation (Noise):", min = 1, max = 15, value = 8, step = 0.5),
      sliderInput("n", "Sample Size (per group):", min = 10, max = 100, value = 30, step = 5),
      
      hr(),
      actionButton("resample", "Draw New Sample", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      plotOutput("boxplot", height = "300px"),
      hr(),
      h3("Pairwise Comparison P-Values"),
      p("Green indicates statistical significance (p < 0.05). Red indicates non-significance (p ≥ 0.05)."),
      tableOutput("pvalue_table")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  
  # Reactive expression to run simulations
  sim_data <- reactive({
    input$resample 
    
    n <- input$n
    sd_val <- input$sd
    
    df <- data.frame(
      group = factor(rep(c("A", "B", "C", "D"), each = n)),
      val = c(rnorm(n, input$mean_A, sd_val),
              rnorm(n, input$mean_B, sd_val),
              rnorm(n, input$mean_C, sd_val),
              rnorm(n, input$mean_D, sd_val))
    )
    
    return(df)
  })
  
  output$boxplot <- renderPlot({
    df <- sim_data()
    
    ggplot(df, aes(x = group, y = val, fill = group)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA) +
      geom_jitter(width = 0.15, size = 1.5, alpha = 0.5) +
      labs(title = "Simulated Group Distributions",
           x = "Group", y = "Value") +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 14)) +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$pvalue_table <- renderTable({
    df <- sim_data()
    
    # Run the ANOVA
    aov_model <- aov(val ~ group, data = df)
    
    # 1. Uncorrected t-tests (using pooled SD to match ANOVA logic)
    pt_none <- pairwise.t.test(df$val, df$group, p.adjust.method = "none", pool.sd = TRUE)$p.value
    
    # 2. Bonferroni corrected
    pt_bonf <- pairwise.t.test(df$val, df$group, p.adjust.method = "bonferroni", pool.sd = TRUE)$p.value
    
    # 3. Tukey HSD
    tukey_res <- TukeyHSD(aov_model)$group
    
    # Helper to format p-values with color
    format_p <- function(p) {
      if (is.na(p)) return("-")
      color <- ifelse(p < 0.05, "green", "red")
      weight <- ifelse(p < 0.05, "bold", "normal")
      paste0("<span style='color:", color, "; font-weight:", weight, ";'>", sprintf("%.4f", p), "</span>")
    }
    
    # Build comparison dataframe manually for clarity
    comparisons <- c("B vs A", "C vs A", "D vs A", "C vs B", "D vs B", "D vs C")
    
    # Extract values from matrices
    # pairwise.t.test matrix rows: B, C, D | cols: A, B, C
    raw_p <- c(pt_none["B", "A"], pt_none["C", "A"], pt_none["D", "A"], 
               pt_none["C", "B"], pt_none["D", "B"], pt_none["D", "C"])
    
    bonf_p <- c(pt_bonf["B", "A"], pt_bonf["C", "A"], pt_bonf["D", "A"], 
                pt_bonf["C", "B"], pt_bonf["D", "B"], pt_bonf["D", "C"])
    
    # Tukey matrix rows are named "B-A", "C-A", etc.
    tuk_p <- c(tukey_res["B-A", "p adj"], tukey_res["C-A", "p adj"], tukey_res["D-A", "p adj"],
               tukey_res["C-B", "p adj"], tukey_res["D-B", "p adj"], tukey_res["D-C", "p adj"])
    
    data.frame(
      `Comparison` = comparisons,
      `Uncorrected t-test` = sapply(raw_p, format_p),
      `Tukey HSD` = sapply(tuk_p, format_p),
      `Bonferroni` = sapply(bonf_p, format_p),
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x) # Allows HTML rendering in the table
}

# Run the application 
shinyApp(ui = ui, server = server)