library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Visualizing Similarity"),
  
  sidebarLayout(
    sidebarPanel(
      p("Move the slider to see how 'Noise' breaks the relationship between X and Y."),
      sliderInput("noise_level", "Amount of Noise (Disorder):", 
                  min = 0, max = 200, value = 50),
      hr(),
      # Adding a toggle for the line helps them see the 'fit' vs 'the dots'
      checkboxInput("show_line", "Show Best Fitting Line", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "500px"),
      h3(textOutput("r_display"), align = "center")
    )
  )
)

server <- function(input, output) {
  
  data_gen <- reactive({
    set.seed(123) # Keep dots in same place, only move them by noise
    x <- seq(1, 100, length.out = 50)
    # y is a direct copy of x, plus the noise
    y <- x + rnorm(50, mean = 0, sd = input$noise_level)
    data.frame(x = x, y = y)
  })
  
  output$scatterPlot <- renderPlot({
    df <- data_gen()
    p <- ggplot(df, aes(x, y)) +
      geom_point(color = "#3498db", size = 3, alpha = 0.7) +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 100), ylim = c(-100, 200)) +
      labs(x = "Variable A", y = "Variable B")
    
    if (input$show_line) {
      p <- p + geom_smooth(method = "lm", color = "#e74c3c", se = FALSE)
    }
    p
  })
  
  output$r_display <- renderText({
    df <- data_gen()
    current_r <- round(cor(df$x, df$y), 2)
    paste0("Pearson's r = ", current_r)
  })
}

shinyApp(ui, server)