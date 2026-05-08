library(shiny)
library(ggplot2)
library(caret)
library(shinythemes)

# 1. DATA SETUP
set.seed(999)
n <- 750
balance <- runif(n, 500, 4500)
# Create a realistic banking scenario
logit_val <- -8 + 0.0035 * balance
prob <- 1 / (1 + exp(-logit_val))
actual <- factor(rbinom(n, 1, prob), levels = c("1", "0"), labels = c("Default", "No Default"))
df_cm <- data.frame(balance = balance, actual = actual, prob = prob)

# 2. UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Model Evaluation: The Confusion Matrix"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Policy Setting"),
      sliderInput("cutoff", "Decision Cut-off (p):", 0, 1, 0.5, step = 0.05),
      hr(),
      helpText("Adjusting this slider changes how the bank classifies 'Risk'."),
      tags$b(""),
      uiOutput("ruleText")
    ),
    
    mainPanel(
      fluidRow(
        column(6, h4("The Confusion Matrix"), tableOutput("cmTable")),
        column(6, h4("Key Performance Metrics"), uiOutput("metricsList"))
      ),
      hr(),
      plotOutput("distPlot"),
      p(tags$small("Note: Blue bars are actual 'No Defaults', Red bars are actual 'Defaults'."))
    )
  )
)

# 3. SERVER
server <- function(input, output) {
  
  # Reactive calculations for the matrix
  results <- reactive({
    preds <- factor(ifelse(df_cm$prob >= input$cutoff, "Default", "No Default"), 
                    levels = c("Default", "No Default"))
    
    # Calculate Matrix components
    tp <- sum(preds == "Default" & df_cm$actual == "Default")
    fp <- sum(preds == "Default" & df_cm$actual == "No Default")
    tn <- sum(preds == "No Default" & df_cm$actual == "No Default")
    fn <- sum(preds == "No Default" & df_cm$actual == "Default")
    
    # Metrics
    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    ppv  <- tp / (tp + fp)
    npv  <- tn / (tn + fn)
    acc  <- (tp + tn) / (tp + tn + fp + fn)
    
    list(tp=tp, fp=fp, tn=tn, fn=fn, sens=sens, spec=spec, ppv=ppv, npv=npv, acc=acc)
  })
  
  output$cmTable <- renderTable({
    res <- results()
    data.frame(
      " " = c("Predicted: DEFAULT", "Predicted: NO DEFAULT"),
      "Actual: DEFAULT" = c(res$tp, res$fn),
      "Actual: NO DEFAULT" = c(res$fp, res$tn)
    )
  }, striped = TRUE, bordered = TRUE, align = 'c')
  
  output$metricsList <- renderUI({
    res <- results()
    tagList(
      tags$ul(
        tags$li(tags$b("Sensitivity: "), round(res$sens*100,1), "% (True Positive Rate)"),
        tags$li(tags$b("Specificity: "), round(res$spec*100,1), "% (True Negative Rate)"),
        tags$li(tags$b("PPV (Precision): "), round(res$ppv*100,1), "% (Predictive Value of +)"),
        tags$li(tags$b("NPV: "), round(res$npv*100,1), "% (Predictive Value of -)"),
        tags$li(tags$b("Accuracy: "), round(res$acc*100,1), "% (Overall Correctness)")
      )
    )
  })
  
  output$distPlot <- renderPlot({
    ggplot(df_cm, aes(x = prob, fill = actual)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      geom_vline(xintercept = input$cutoff, linetype = "dashed", size = 1.2) +
      scale_fill_manual(values = c("Default" = "#e74c3c", "No Default" = "#3498db")) +
      labs(title = "Probability Distribution & The Decision Cut-off",
           x = "Predicted Probability of Default", y = "Count of Customers") +
      theme_minimal()
  })
}

shinyApp(ui, server)