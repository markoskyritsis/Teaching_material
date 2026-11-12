library(shiny)
library(ggplot2)
library(carData) # Contains the Salaries dataset
library(dplyr)

# Load and prep data
data("Salaries")

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Case Study: What Actually Drives Professor Salaries?"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("model_type", "Select Your Model View:",
                   choices = list(
                     "1. Simple View: Years of Service only" = "simple",
                     "2. Simple View: Rank only" = "rank_only",
                     "3. Multiple View: Combine Both" = "multiple"
                   )),
      hr(),
      helpText("Watch how the 'Significance' of Years of Service changes when Rank is added to the room.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisation", plotOutput("salaryPlot")),
        tabPanel("The Results (Textbook Summary)", verbatimTextOutput("modelSummary")),
        tabPanel("Plain English Interpretation", uiOutput("interpretation"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive model building
  current_model <- reactive({
    if (input$model_type == "simple") {
      return(lm(salary ~ yrs.service, data = Salaries))
    } else if (input$model_type == "rank_only") {
      return(lm(salary ~ rank, data = Salaries))
    } else {
      return(lm(salary ~ yrs.service + rank, data = Salaries))
    }
  })
  
  # 1. Visualization
  output$salaryPlot <- renderPlot({
    p <- ggplot(Salaries, aes(y = salary))
    
    if (input$model_type == "simple") {
      p <- p + aes(x = yrs.service) + 
        geom_point(alpha = 0.5, color = "steelblue") + 
        geom_smooth(method = "lm", color = "darkred") +
        labs(title = "Does Experience = More Money?", x = "Years of Service")
      
    } else if (input$model_type == "rank_only") {
      p <- p + aes(x = rank, fill = rank) + 
        geom_boxplot() + 
        labs(title = "Does Job Title = More Money?", x = "Rank")
      
    } else {
      # For the multiple view, we show Years vs Salary, but colored by Rank
      p <- p + aes(x = yrs.service, color = rank) + 
        geom_point(alpha = 0.6) + 
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Experience vs. Salary (Colored by Rank)", x = "Years of Service")
    }
    p + theme_minimal() + scale_y_continuous(labels = scales::dollar)
  })
  
  # 2. Statistical Output
  output$modelSummary <- renderPrint({
    summary(current_model())
  })
  
  # 3. Plain English Interpretation
  output$interpretation <- renderUI({
    m <- current_model()
    s <- summary(m)
    
    if (input$model_type == "simple") {
      tagList(
        h4("What are we seeing?"),
        p("In this simple view, Years of Service looks like a very strong predictor. The p-value is tiny, and it seems like every extra year of service adds a significant amount to your paycheck."),
        strong("The Trap:"), p("We are assuming 'Time' is the only thing that matters.")
      )
    } else if (input$model_type == "rank_only") {
      tagList(
        h4("What are we seeing?"),
        p("Rank is an even stronger predictor. Moving from Assistant to Associate to Full Professor comes with massive jumps in salary."),
        p("But do we need both variables?")
      )
    } else {
      tagList(
        h4("(Multiple Regression)"),
        p("Look at the coefficients table in the other tab. Notice that the p-value for 'yrs.service' has likely become much less significant (or vanished)."),
        strong("Conclusion:"), 
        p("When we control for Rank, Years of Service is no longer a major driver. This proves that you don't get paid more just for getting older; you get paid more because you get promoted. Rank is the mediator.")
      )
    }
  })
}

shinyApp(ui, server)