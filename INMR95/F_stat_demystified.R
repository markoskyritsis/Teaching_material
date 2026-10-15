library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  withMathJax(), # Enable math rendering
  
  titlePanel("Deconstructing the F-Statistic"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Group Means (The Signal)"),
      sliderInput("mean_A", "Group A Mean:", min = 40, max = 60, value = 45, step = 1),
      sliderInput("mean_B", "Group B Mean:", min = 40, max = 60, value = 50, step = 1),
      sliderInput("mean_C", "Group C Mean:", min = 40, max = 60, value = 55, step = 1),
      
      hr(),
      h4("Shared Variability (The Noise)"),
      sliderInput("sd_shared", "Standard Deviation (Within-Group):", 
                  min = 1, max = 15, value = 5, step = 0.5),
      
      hr(),
      helpText("Sample size is fixed at n = 20 per group for this demonstration.")
    ),
    
    mainPanel(
      h3("Visualizing Variance"),
      p("The dashed black line is the Grand Mean. The colored solid lines are the Group Means."),
      plotOutput("anova_plot", height = "400px"),
      
      hr(),
      h3("The Calculation"),
      uiOutput("calculations")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  
  # Reactive data generation
  sim_data <- reactive({
    # Fix seed so points only move based on parameters, reducing visual jitter distraction
    set.seed(42) 
    
    n <- 20
    
    data.frame(
      Group = factor(rep(c("A", "B", "C"), each = n)),
      Value = c(
        rnorm(n, mean = input$mean_A, sd = input$sd_shared),
        rnorm(n, mean = input$mean_B, sd = input$sd_shared),
        rnorm(n, mean = input$mean_C, sd = input$sd_shared)
      )
    )
  })
  
  # Render the plot
  output$anova_plot <- renderPlot({
    df <- sim_data()
    grand_mean <- mean(df$Value)
    
    group_means <- df %>%
      group_by(Group) %>%
      summarise(Group_Mean = mean(Value), .groups = 'drop')
    
    ggplot(df, aes(x = Group, y = Value, color = Group)) +
      geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
      geom_hline(yintercept = grand_mean, linetype = "dashed", color = "black", size = 1) +
      geom_segment(data = group_means, 
                   aes(x = as.numeric(Group) - 0.3, xend = as.numeric(Group) + 0.3, 
                       y = Group_Mean, yend = Group_Mean, color = Group), 
                   size = 1.5) +
      scale_y_continuous(limits = c(20, 80)) +
      labs(y = "Score", x = "Group") +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 16))
  })
  
  # Render calculations
  output$calculations <- renderUI({
    df <- sim_data()
    n <- 20
    k <- 3
    N <- n * k
    
    grand_mean <- mean(df$Value)
    
    # Calculate Sums of Squares
    group_stats <- df %>%
      group_by(Group) %>%
      summarise(mean = mean(Value), var = var(Value), .groups = 'drop')
    
    SSB <- sum(n * (group_stats$mean - grand_mean)^2)
    SSW <- sum((n - 1) * group_stats$var)
    
    # Degrees of Freedom
    df_B <- k - 1
    df_W <- N - k
    
    # Mean Squares
    MSB <- SSB / df_B
    MSW <- SSW / df_W
    
    # F-statistic
    F_stat <- MSB / MSW
    
    HTML(paste0(
      "<div style='font-size: 16px; font-family: monospace; background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
      "<b>1. Between-Group Variance (Mean Square Between)</b><br/>",
      "Spread of the group means around the grand mean.<br/>",
      "MS<sub>between</sub> = ", round(MSB, 2), "<br/><br/>",
      
      "<b>2. Within-Group Variance (Mean Square Within)</b><br/>",
      "Average spread of individual points around their own group mean.<br/>",
      "MS<sub>within</sub>  = ", round(MSW, 2), "<br/><br/>",
      
      "<b>3. The F-Ratio</b><br/>",
      "F = MS<sub>between</sub> / MS<sub>within</sub><br/>",
      "F = ", round(MSB, 2), " / ", round(MSW, 2), " = <b><span style='color: red; font-size: 1.2em;'>", round(F_stat, 2), "</span></b>",
      "</div>"
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)