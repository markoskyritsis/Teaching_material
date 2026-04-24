library(shiny)
library(ggplot2)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Categorical Predictors: Ice Cream Sales"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("summer_boost", "Average Summer Sales Boost ($):", 
                  min = 0, max = 500, value = 250, step = 10),
      sliderInput("noise", "Daily Variation (Noise):", 
                  min = 10, max = 150, value = 50, step = 10),
      hr(),
      p("In this model, 'Winter' is the reference group (0) and 'Summer' is the dummy variable (1).")
    ),
    
    mainPanel(
      plotOutput("dummyPlot"),
      h4("The R Output:"),
      verbatimTextOutput("summaryOut"),
      wellPanel(
        h5("How to read this:"),
        uiOutput("explanation")
      )
    )
  )
)

server <- function(input, output) {
  
  data_react <- reactive({
    set.seed(123)
    n <- 40 # 20 days of winter, 20 days of summer
    
    season <- rep(c("Winter", "Summer"), each = 20)
    # Winter baseline = 100. Summer = 100 + boost
    base_sales <- 100
    sales <- ifelse(season == "Winter", 
                    base_sales + rnorm(20, 0, input$noise),
                    base_sales + input$summer_boost + rnorm(20, 0, input$noise))
    
    data.frame(Season = factor(season, levels = c("Winter", "Summer")), Sales = sales)
  })
  
  output$dummyPlot <- renderPlot({
    df <- data_react()
    ggplot(df, aes(x = Season, y = Sales, color = Season)) +
      geom_jitter(width = 0.1, size = 3, alpha = 0.7) +
      stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") +
      theme_minimal() +
      labs(title = "Sales by Season", subtitle = "The black bars represent the group means (y-hat)")
  })
  
  output$summaryOut <- renderPrint({
    model <- lm(Sales ~ Season, data = data_react())
    summary(model)
  })
  
  output$explanation <- renderUI({
    fit <- lm(Sales ~ Season, data = data_react())
    b0 <- round(coef(fit)[1], 2)
    b1 <- round(coef(fit)[2], 2)
    
    HTML(paste0(
      "<b>Intercept (b0):</b> ", b0, " — This is the average sales in <b>Winter</b>.<br>",
      "<b>SeasonSummer (b1):</b> ", b1, " — This is how much <b>more</b> we sell in Summer compared to Winter."
    ))
  })
}

shinyApp(ui = ui, server = server)