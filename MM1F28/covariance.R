library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Covariance vs. Correlation: The Power of Standardisation"),
  
  sidebarLayout(
    sidebarPanel(
      p("1. Adjust the relationship strength:"),
      sliderInput("strength", "Relationship Strength:", min = 0, max = 1, value = 0.7, step = 0.1),
      
      hr(),
      p("2. Change the 'Scale' (Units):"),
      radioButtons("scale", "Measurement Units:",
                   choices = list("Dollars ($)" = 1, 
                                  "Cents (¢)" = 100, 
                                  "Micro-units" = 10000),
                   selected = 1),
      
      helpText("Notice how Covariance explodes when units change, but Correlation stays the same!")
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      wellPanel(
        fluidRow(
          column(6, h4("Covariance:"), h2(textOutput("cov_val"), style="color: #e67e22;")),
          column(6, h4("Pearson's r (Correlation):"), h2(textOutput("cor_val"), style="color: #2980b9;"))
        )
      ),
      p(em("Standardising the covariance by the standard deviations of X and Y gives us 'r', which is always between -1 and 1."))
    )
  )
)

server <- function(input, output) {
  
  data_gen <- reactive({
    set.seed(42)
    n <- 100
    x <- rnorm(n, 50, 10)
    # y is x + some noise, then scaled by the 'Units'
    noise <- rnorm(n, 0, (1 - input$strength) * 20)
    y <- (x + noise) * as.numeric(input$scale)
    data.frame(x = x, y = y)
  })
  
  output$scatterPlot <- renderPlot({
    df <- data_gen()
    ggplot(df, aes(x, y)) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      geom_smooth(method = "lm", color = "#2980b9", se = FALSE) +
      theme_minimal() +
      labs(title = "Data Plot (Scale changes with Units)")
  })
  
  output$cov_val <- renderText({
    df <- data_gen()
    round(cov(df$x, df$y), 2)
  })
  
  output$cor_val <- renderText({
    df <- data_gen()
    round(cor(df$x, df$y), 3)
  })
}

shinyApp(ui, server)