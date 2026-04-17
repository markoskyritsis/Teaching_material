library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("When Pearson is Blind: Linear vs. Quadratic"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("shape", "Select the Relationship:",
                   choices = c("Linear (Straight Line)" = "lin", 
                               "Quadratic (U-Shape)" = "quad")),
      sliderInput("noise", "Amount of Noise:", min = 0, max = 50, value = 5),
      hr(),
      p("Pearson's r only measures 'Linear' strength. Watch what happens to the score when the shape is a curve!")
    ),
    
    mainPanel(
      plotOutput("distPlot", height = "500px"),
      wellPanel(
        h3(textOutput("corText"), align = "center")
      ),
      p(em("Note: Even if the U-shape is 'perfect' (zero noise), Pearson will often report a correlation of 0. This is why we MUST visualise data!"))
    )
  )
)

server <- function(input, output) {
  
  data_gen <- reactive({
    set.seed(123)
    x <- seq(-100, 100, length.out = 100)
    
    if(input$shape == "lin") {
      y <- x + rnorm(100, 0, input$noise)
    } else {
      # Perfect U-shape: y = x^2
      # We scale it down so the axis isn't too extreme
      y <- (x^2 / 50) + rnorm(100, 0, input$noise)
    }
    data.frame(x = x, y = y)
  })
  
  output$distPlot <- renderPlot({
    df <- data_gen()
    ggplot(df, aes(x, y)) +
      geom_point(color = "#2c3e50", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "#e74c3c", se = FALSE, linetype = "dashed") +
      theme_minimal() +
      labs(title = paste("Relationship Type:", ifelse(input$shape == "lin", "Linear", "Quadratic")),
           subtitle = "Red dashed line = What Pearson 'sees' (the best fitting straight line)")
  })
  
  output$corText <- renderText({
    df <- data_gen()
    r_val <- round(cor(df$x, df$y), 3)
    paste0("Pearson's r = ", r_val)
  })
}

shinyApp(ui, server)