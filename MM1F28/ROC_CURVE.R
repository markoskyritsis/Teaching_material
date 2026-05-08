library(shiny)
library(ggplot2)
library(pROC)
library(shinythemes)

# 1. DATA SETUP
set.seed(888)
n <- 750
balance <- runif(n, 500, 4000)
student <- sample(c(0, 1), n, replace = TRUE)
# Logic: Strong balance effect, moderate student reduction
logit_val <- -10 + 0.0045 * balance - 1.2 * student
prob <- 1 / (1 + exp(-logit_val))
default <- rbinom(n, 1, prob)
df_roc <- data.frame(balance = balance, student = student, default = default, prob = prob)

# Pre-calculate ROC
roc_obj <- roc(df_roc$default, df_roc$prob)

# 2. UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Banking Strategy: The ROC Curve"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Management Strategy"),
      sliderInput("threshold", "Decision Threshold (p):", 0, 1, 0.5, step = 0.05),
      hr(),
      p("If the predicted probability is above this line, we predict 'DEFAULT'."),
      wellPanel(
        h5("Current Performance"),
        uiOutput("statSummary")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. The ROC Curve", 
                 plotOutput("rocPlot"),
                 p("The curve shows the trade-off between Sensitivity and Specificity at EVERY possible threshold.")),
        tabPanel("2. Sensitivity vs Specificity", 
                 plotOutput("tradeoffPlot"),
                 p("Notice how increasing the threshold makes the bank 'Safer' but misses more defaults."))
      )
    )
  )
)

# 3. SERVER
server <- function(input, output) {
  
  # Calculate performance based on user threshold
  performance <- reactive({
    preds <- ifelse(df_roc$prob >= input$threshold, 1, 0)
    tp <- sum(preds == 1 & df_roc$default == 1)
    fp <- sum(preds == 1 & df_roc$default == 0)
    tn <- sum(preds == 0 & df_roc$default == 0)
    fn <- sum(preds == 0 & df_roc$default == 1)
    
    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    
    list(sens = sens, spec = spec, auc = auc(roc_obj))
  })
  
  output$rocPlot <- renderPlot({
    perf <- performance()
    
    # Plotting the ROC curve
    plot(roc_obj, col = "#2c3e50", lwd = 4, main = paste("Model AUC:", round(perf$auc, 3)))
    # Add a point for the current threshold
    points(x = perf$spec, y = perf$sens, col = "red", pch = 19, cex = 3)
    legend("bottomright", legend = c("All Thresholds", "Your Current Policy"), 
           col = c("#2c3e50", "red"), lwd = c(4, NA), pch = c(NA, 19))
  })
  
  output$statSummary <- renderUI({
    perf <- performance()
    tagList(
      p(tags$b("Sensitivity (True Positive Rate): "), round(perf$sens * 100, 1), "%"),
      p(tags$b("Specificity (True Negative Rate): "), round(perf$spec * 100, 1), "%"),
      hr(),
      helpText("Higher threshold = Fewer false alarms, but more missed defaults.")
    )
  })
  
  output$tradeoffPlot <- renderPlot({
    # Visualization of the threshold trade-off
    thresholds <- seq(0, 1, 0.01)
    metrics <- data.frame(
      t = thresholds,
      sens = sapply(thresholds, function(t) sum(df_roc$prob >= t & df_roc$default == 1) / sum(df_roc$default == 1)),
      spec = sapply(thresholds, function(t) sum(df_roc$prob < t & df_roc$default == 0) / sum(df_roc$default == 0))
    )
    
    ggplot(metrics, aes(x = t)) +
      geom_line(aes(y = sens, color = "Sensitivity"), size = 1) +
      geom_line(aes(y = spec, color = "Specificity"), size = 1) +
      geom_vline(xintercept = input$threshold, linetype = "dashed", color = "red") +
      labs(title = "The Management Trade-off", x = "Threshold (p)", y = "Rate") +
      theme_minimal()
  })
}

shinyApp(ui, server)