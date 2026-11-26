# Required packages:
# install.packages(c("shiny", "rpart", "rpart.plot", "pROC", "caret", "ggplot2"))

library(shiny)
library(rpart)
library(rpart.plot)
library(pROC)
library(caret)
library(ggplot2)

# Helper to generate datasets
get_dataset <- function(choice) {
  if (choice == "iris_mid") {
    df <- iris[, c("Petal.Length", "Petal.Width")]
    df$Target <- factor(ifelse(iris$Species == "versicolor", "Yes", "No"),
                        levels = c("No", "Yes"))
    return(list(data = df, is_2d = TRUE, x_var = "Petal.Length", y_var = "Petal.Width",
                desc = "Predicting Versicolor vs. Others: sits in the middle of petal space (non-monotonic)."))
  }
  
  if (choice == "xor_rules") {
    set.seed(42)
    n <- 400
    x1 <- runif(n, -3, 3)
    x2 <- runif(n, -3, 3)
    # Positive class in top-right and bottom-left quadrants (with a little noise)
    true_rule <- (x1 > 0 & x2 > 0) | (x1 < 0 & x2 < 0)
    flip <- rbinom(n, 1, 0.08) == 1
    y <- ifelse(flip, !true_rule, true_rule)
    df <- data.frame(
      Feature_A = x1,
      Feature_B = x2,
      Target = factor(ifelse(y, "Yes", "No"), levels = c("No", "Yes"))
    )
    return(list(data = df, is_2d = TRUE, x_var = "Feature_A", y_var = "Feature_B",
                desc = "Simulated XOR rule: Target = Yes when both features share the same sign."))
  }
  
  if (choice == "titanic") {
    raw <- as.data.frame(datasets::Titanic)
    df <- raw[rep(seq_len(nrow(raw)), raw$Freq), 1:4]
    df$Target <- factor(df$Survived, levels = c("No", "Yes"))
    df$Survived <- NULL
    rownames(df) <- NULL
    return(list(data = df, is_2d = FALSE, x_var = NULL, y_var = NULL,
                desc = "Base R Titanic: survival driven by Sex x Class x Age interactions."))
  }
}

# Helper to compute confusion matrix summary
get_cm_stats <- function(probs, actual, threshold) {
  preds <- factor(ifelse(probs >= threshold, "Yes", "No"), levels = c("No", "Yes"))
  cm <- caret::confusionMatrix(preds, actual, positive = "Yes")
  
  # Coordinates for the active threshold point on ROC
  tpr <- as.numeric(cm$byClass["Sensitivity"])
  fpr <- 1 - as.numeric(cm$byClass["Specificity"])
  
  list(
    cm = cm,
    table_df = as.data.frame.matrix(cm$table),
    acc = as.numeric(cm$overall["Accuracy"]),
    sens = tpr,
    spec = as.numeric(cm$byClass["Specificity"]),
    fpr = fpr
  )
}

ui <- fluidPage(
  titlePanel("CART vs. Binomial Logistic Regression"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "dataset", "Dataset:",
        choices = c(
          "Iris (Versicolor vs. Others)" = "iris_mid",
          "Simulated Rule-Based (XOR)"   = "xor_rules",
          "Titanic (Survival)"           = "titanic"
        )
      ),
      helpText(textOutput("dataset_desc")),
      hr(),
      sliderInput("threshold", "Classification Threshold:",
                  min = 0.05, max = 0.95, value = 0.50, step = 0.05),
      sliderInput("cp", "CART Pruning Penalty (cp):",
                  min = 0.00, max = 0.15, value = 0.01, step = 0.005),
      sliderInput("split_prop", "Training Set Proportion:",
                  min = 0.50, max = 0.85, value = 0.70, step = 0.05),
      numericInput("seed", "Random Split Seed:", value = 123, min = 1, max = 9999)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "ROC & Confusion Matrices",
          br(),
          plotOutput("roc_plot", height = "380px"),
          hr(),
          fluidRow(
            column(
              6,
              h4("CART Confusion Matrix (Test Set)"),
              tableOutput("cm_cart_table"),
              verbatimTextOutput("cm_cart_metrics")
            ),
            column(
              6,
              h4("Logistic Regression Confusion Matrix (Test Set)"),
              tableOutput("cm_glm_table"),
              verbatimTextOutput("cm_glm_metrics")
            )
          )
        ),
        tabPanel(
          "Why CART Wins (Tree & Boundaries)",
          br(),
          fluidRow(
            column(6, h4("Fitted CART Structure"), plotOutput("tree_plot", height = "420px")),
            column(6, h4("Feature Space & Decision Boundaries"), plotOutput("boundary_plot", height = "420px"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ds <- reactive({ get_dataset(input$dataset) })
  
  output$dataset_desc <- renderText({ ds()$desc })
  
  # Fit both models on the same stratified train/test split
  models <- reactive({
    df <- ds()$data
    set.seed(input$seed)
    idx <- caret::createDataPartition(df$Target, p = input$split_prop, list = FALSE)
    train_df <- df[idx, ]
    test_df  <- df[-idx, ]
    
    # 1. CART
    cart_fit <- rpart(
      Target ~ ., data = train_df, method = "class",
      control = rpart.control(cp = input$cp)
    )
    cart_probs <- predict(cart_fit, newdata = test_df, type = "prob")[, "Yes"]
    
    # 2. Binomial Logistic Regression (main effects)
    glm_fit <- glm(Target ~ ., data = train_df, family = binomial)
    glm_probs <- predict(glm_fit, newdata = test_df, type = "response")
    
    # ROC objects (explicit levels & direction prevent flipping when AUC < 0.5)
    roc_cart <- pROC::roc(test_df$Target, cart_probs,
                          levels = c("No", "Yes"), direction = "<", quiet = TRUE)
    roc_glm  <- pROC::roc(test_df$Target, glm_probs,
                          levels = c("No", "Yes"), direction = "<", quiet = TRUE)
    
    list(
      train = train_df, test = test_df,
      cart_fit = cart_fit, cart_probs = cart_probs, roc_cart = roc_cart,
      glm_fit  = glm_fit,  glm_probs  = glm_probs,  roc_glm  = roc_glm
    )
  })
  
  # Reactive confusion matrices updated by threshold slider
  cm_cart <- reactive({ get_cm_stats(models()$cart_probs, models()$test$Target, input$threshold) })
  cm_glm  <- reactive({ get_cm_stats(models()$glm_probs,  models()$test$Target, input$threshold) })
  
  # Overlaid ROC Curve
  output$roc_plot <- renderPlot({
    m <- models()
    auc_cart <- round(as.numeric(pROC::auc(m$roc_cart)), 3)
    auc_glm  <- round(as.numeric(pROC::auc(m$roc_glm)), 3)
    
    lbl_cart <- paste0("CART (AUC = ", auc_cart, ")")
    lbl_glm  <- paste0("Logistic Regression (AUC = ", auc_glm, ")")
    
    roc_df <- rbind(
      data.frame(FPR = 1 - m$roc_cart$specificities, TPR = m$roc_cart$sensitivities, Model = lbl_cart),
      data.frame(FPR = 1 - m$roc_glm$specificities,  TPR = m$roc_glm$sensitivities,  Model = lbl_glm)
    )
    roc_df <- roc_df[order(roc_df$Model, roc_df$FPR, roc_df$TPR), ]
    
    pts_df <- data.frame(
      FPR = c(cm_cart()$fpr, cm_glm()$fpr),
      TPR = c(cm_cart()$sens, cm_glm()$sens),
      Model = c(lbl_cart, lbl_glm)
    )
    
    ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
      geom_path(linewidth = 1.3) +
      geom_point(data = pts_df, aes(x = FPR, y = TPR, color = Model), size = 4.5) +
      scale_color_manual(values = c("#1b9e77", "#d95f02")) +
      coord_equal() +
      labs(
        title = "ROC Curve Comparison (Test Set)",
        subtitle = paste0("Dots mark the current classification threshold (p >= ", input$threshold, ")"),
        x = "False Positive Rate (1 - Specificity)",
        y = "True Positive Rate (Sensitivity)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", legend.title = element_blank())
  })
  
  # Confusion Matrix Tables (Rows = Predicted, Columns = Actual)
  output$cm_cart_table <- renderTable({ cm_cart()$table_df }, rownames = TRUE)
  output$cm_glm_table  <- renderTable({ cm_glm()$table_df },  rownames = TRUE)
  
  output$cm_cart_metrics <- renderText({
    s <- cm_cart()
    sprintf("Accuracy:    %.3f\nSensitivity: %.3f\nSpecificity: %.3f", s$acc, s$sens, s$spec)
  })
  
  output$cm_glm_metrics <- renderText({
    s <- cm_glm()
    sprintf("Accuracy:    %.3f\nSensitivity: %.3f\nSpecificity: %.3f", s$acc, s$sens, s$spec)
  })
  
  # Tab 2: Tree Plot
  output$tree_plot <- renderPlot({
    fit <- models()$cart_fit
    if (nrow(fit$frame) > 1) {
      rpart.plot(fit, type = 4, extra = 104, fallen.leaves = TRUE, box.palette = "GnBu")
    } else {
      plot.new()
      text(0.5, 0.5, "Tree pruned to a single root node.\nLower cp to allow splits.", cex = 1.2)
    }
  })
  
  # Tab 2: 2D Boundary Comparison (for Iris and XOR)
  output$boundary_plot <- renderPlot({
    info <- ds()
    m <- models()
    
    if (!info$is_2d) {
      plot.new()
      text(0.5, 0.5, "2D boundary plot is shown for continuous\n2-feature datasets (Iris and XOR).", cex = 1.2)
      return()
    }
    
    x_var <- info$x_var
    y_var <- info$y_var
    df_all <- info$data
    
    grid <- expand.grid(
      x = seq(min(df_all[[x_var]]) - 0.3, max(df_all[[x_var]]) + 0.3, length.out = 150),
      y = seq(min(df_all[[y_var]]) - 0.3, max(df_all[[y_var]]) + 0.3, length.out = 150)
    )
    names(grid) <- c(x_var, y_var)
    
    grid$cart_prob <- predict(m$cart_fit, newdata = grid, type = "prob")[, "Yes"]
    grid$glm_prob  <- predict(m$glm_fit,  newdata = grid, type = "response")
    
    ggplot() +
      geom_raster(data = grid, aes(x = .data[[x_var]], y = .data[[y_var]],
                                   fill = cart_prob >= input$threshold), alpha = 0.25) +
      geom_contour(data = grid, aes(x = .data[[x_var]], y = .data[[y_var]], z = glm_prob),
                   breaks = input$threshold, color = "#d95f02", linewidth = 1.2, linetype = "dashed") +
      geom_point(data = m$test, aes(x = .data[[x_var]], y = .data[[y_var]], color = Target),
                 size = 2.5, alpha = 0.85) +
      scale_fill_manual(values = c("FALSE" = "grey85", "TRUE" = "#1b9e77"), guide = "none") +
      scale_color_manual(values = c("No" = "grey30", "Yes" = "#1b9e77")) +
      labs(
        title = "CART Regions (Shaded) vs. Logistic Boundary (Dashed Line)",
        subtitle = "Points shown are the held-out test set",
        x = x_var, y = y_var
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)