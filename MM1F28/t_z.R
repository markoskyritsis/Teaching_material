library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("The Convergence of t to Z"),
  sidebarLayout(
    sidebarPanel(
      helpText("Adjust the sample size to see how the t-distribution 'stretches' 
               to account for uncertainty."),
      sliderInput("n", "Sample Size (n):", 
                  min = 2, max = 150, value = 5, step = 1),
      hr(),
      wellPanel(
        h4("95% Multiplier (k)"),
        textOutput("multiplier_info")
      )
    ),
    mainPanel(
      plotOutput("distPlot"),
      p("The blue area represents the standard Normal (Z) distribution. 
         The red dashed line is the t-distribution for your current sample size.")
    )
  )
)

server <- function(input, output) {
  
  output$multiplier_info <- renderText({
    t_val <- qt(0.975, df = input$n - 1)
    diff_pct <- ((t_val - 1.96) / 1.96) * 100
    paste0("t-multiplier: ", round(t_val, 3), 
           "\n(Z-multiplier is 1.960)",
           "\n\nDifference: ", round(diff_pct, 1), "%")
  })
  
  output$distPlot <- renderPlot({
    df_val <- input$n - 1
    
    ggplot(data.frame(x = c(-4, 4)), aes(x)) +
      # Normal Distribution Fill
      stat_function(fun = dnorm, geom = "area", fill = "steelblue", alpha = 0.2) +
      stat_function(fun = dnorm, aes(color = "Z (Normal)"), size = 1) +
      # T Distribution Line
      stat_function(fun = dt, args = list(df = df_val), 
                    aes(color = paste("t (df =", df_val, ")")), 
                    size = 1.2, linetype = "dashed") +
      # Vertical lines for 95% tails (Z)
      geom_vline(xintercept = c(-1.96, 1.96), color = "steelblue", linetype = "dotted") +
      scale_color_manual(values = c("firebrick", "steelblue")) +
      labs(title = paste("n =", input$n, "| Comparison of Densities"),
           subtitle = "Dotted vertical lines show the 1.96 Z-threshold",
           x = "Standard Deviations", y = "Density") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)