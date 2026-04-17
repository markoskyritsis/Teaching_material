library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Step 1: Visualizing Relationships in the Wild"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("case", "Select a Case Study:", 
                  choices = c("Accounting: Sales vs. Tax", 
                              "Economics: Price vs. Demand", 
                              "Finance: Risk vs. Return", 
                              "HR: Experience vs. Task Time",
                              "Biology: Kibble vs. Cat Weight")),
      hr(),
      sliderInput("noise_level", "Current Noise (Adjust to see impact):", 
                  min = 0, max = 200, value = 50),
      checkboxInput("show_line", "Show Best Fitting Line", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "500px"),
      h3(textOutput("r_display"), align = "center"),
      p(em(textOutput("description")))
    )
  )
)

server <- function(input, output, session) {
  
  # Update noise slider based on the case study selected
  observeEvent(input$case, {
    new_val <- switch(input$case,
                      "Accounting: Sales vs. Tax" = 2,
                      "Economics: Price vs. Demand" = 40,
                      "Finance: Risk vs. Return" = 80,
                      "HR: Experience vs. Task Time" = 60,
                      "Biology: Kibble vs. Cat Weight" = 30)
    updateSliderInput(session, "noise_level", value = new_val)
  })
  
  data_gen <- reactive({
    set.seed(42)
    n <- 50
    x <- seq(1, 100, length.out = n)
    
    # Logic for positive vs negative relationships
    slope <- if(input$case %in% c("Economics: Price vs. Demand", "HR: Experience vs. Task Time")) -1 else 1
    
    y <- (slope * x) + rnorm(n, mean = 0, sd = input$noise_level)
    data.frame(x = x, y = y)
  })
  
  output$scatterPlot <- renderPlot({
    df <- data_gen()
    labels <- switch(input$case,
                     "Accounting: Sales vs. Tax" = list(x = "Gross Sales ($)", y = "Tax Collected ($)"),
                     "Economics: Price vs. Demand" = list(x = "Price ($)", y = "Quantity Sold"),
                     "Finance: Risk vs. Return" = list(x = "Volatility (Risk %)", y = "Annual Return (%)"),
                     "HR: Experience vs. Task Time" = list(x = "Years of Experience", y = "Minutes to Complete Task"),
                     "Biology: Kibble vs. Cat Weight" = list(x = "Daily Kibble (Grams)", y = "Cat Weight (kg)"))
    
    ggplot(df, aes(x, y)) +
      geom_point(color = "#2c3e50", size = 3, alpha = 0.6) +
      labs(x = labels$x, y = labels$y) +
      theme_light() +
      if(input$show_line) geom_smooth(method = "lm", color = "#e67e22", se = FALSE)
  })
  
  output$r_display <- renderText({
    df <- data_gen()
    paste0("Pearson's r = ", round(cor(df$x, df$y), 2))
  })
  
  output$description <- renderText({
    switch(input$case,
           "Biology: Kibble vs. Cat Weight" = "A classic positive correlation. More fuel usually means more cat, but metabolism and zoomies provide the noise!")
  })
}

shinyApp(ui, server)