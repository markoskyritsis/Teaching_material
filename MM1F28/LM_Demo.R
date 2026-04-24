library(shiny)
library(ggplot2)

ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("Deconstructing the R Linear Model Output"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("noise_lvl", "Random Noise (Level of Uncertainty):", 
                  min = 10, max = 500, value = 50, step = 10),
      hr(),
      helpText("Watch how the 'Significance' codes and p-values change as the relationship gets messier.")
    ),
    
    mainPanel(
      plotOutput("regPlot"),
      h4("The 'R' Console Output (summary(lm(...))):"),
      verbatimTextOutput("modelSummary"),
      wellPanel(
        p("Focus on the 'Coefficients' table and the 'Multiple R-squared' at the bottom.")
      )
    )
  )
)

server <- function(input, output) {
  
  data_react <- reactive({
    set.seed(123)
    n <- 50
    budget <- seq(100, 1000, length.out = n)
    # Underlying truth: Sales = 200 + 1.5 * budget + noise
    sales <- 200 + 1.5 * budget + rnorm(n, mean = 0, sd = input$noise_lvl)
    data.frame(budget = budget, sales = sales)
  })
  
  output$regPlot <- renderPlot({
    ggplot(data_react(), aes(x = budget, y = sales)) +
      geom_point(color = "steelblue", size = 3) +
      geom_smooth(method = "lm", color = "darkorange", se = TRUE) +
      theme_minimal() +
      labs(x = "Marketing Budget ($)", y = "Sales ($)")
  })
  
  output$modelSummary <- renderPrint({
    model <- lm(sales ~ budget, data = data_react())
    summary(model)
  })
}

shinyApp(ui = ui, server = server)