library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Understanding R-Squared"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("r_val", "Correlation (r):", 
                  min = 0, max = 1, value = 0.7, step = 0.01),
      hr(),
      h4("The Math:"),
      uiOutput("math_conversion"),
      hr(),
      h4("The Interpretation:"),
      uiOutput("interpretation")
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      wellPanel(
        p("As you increase r, the points huddle closer to the line. 
          The R-squared tells us what % of the vertical 'spread' 
          is captured by our model.")
      )
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    set.seed(42)
    n <- 150
    x <- seq(1, 100, length.out = n)
    
    # Generate y based on r
    # We use the relationship: var_explained = r^2
    r <- input$r_val
    y <- r * x + sqrt(1 - r^2) * rnorm(n, mean = 50, sd = 30)
    
    df <- data.frame(x = x, y = y)
    
    ggplot(df, aes(x, y)) +
      geom_point(alpha = 0.5, color = "darkblue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(title = paste("Scatter Plot (r =", r, ")"),
           x = "Marketing Budget", y = "Sales")
  })
  
  output$math_conversion <- renderUI({
    r_sq <- round(input$r_val^2, 3)
    withMathJax(
      paste0("$$r = ", input$r_val, "$$"),
      paste0("$$R^2 = r^2 = ", r_sq, "$$")
    )
  })
  
  output$interpretation <- renderUI({
    r_sq_pct <- round(input$r_val^2 * 100, 1)
    HTML(paste0("Our model explains <b>", r_sq_pct, "%</b> of the variation in Sales.<br><br>",
                "The remaining <b>", 100 - r_sq_pct, "%</b> is 'Error' (unexplained noise)."))
  })
}

shinyApp(ui = ui, server = server)