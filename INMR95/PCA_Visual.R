library(shiny)
library(ggplot2)

# 1. Generate Correlated Data (Mean-Centered)
set.seed(42)
x_raw <- rnorm(150, mean = 0, sd = 3)
y_raw <- 0.8 * x_raw + rnorm(150, mean = 0, sd = 1.5)

# PCA requires mean-centered data
df <- data.frame(
  x = x_raw - mean(x_raw),
  y = y_raw - mean(y_raw)
)

# 2. Pre-calculate the actual PC1 to trigger the "Aha!" moment visual changes
pca_result <- prcomp(df)
pc1_vector <- pca_result$rotation[, "PC1"]
pc1_angle_rad <- atan2(pc1_vector["y"], pc1_vector["x"])
pc1_angle_deg <- (pc1_angle_rad * 180 / pi) %% 180
max_variance <- pca_result$sdev[1]^2

# 3. Define UI
ui <- fluidPage(
  titlePanel("Principal Component Analysis: The Search for Variance"),
  
  sidebarLayout(
    sidebarPanel(
      p("Rotate the line through the data cloud. Your goal is to maximize the spread (variance) of the red dots."),
      sliderInput("angle", 
                  "Angle of Line (Degrees):", 
                  min = 0, 
                  max = 180, 
                  value = 0, 
                  step = 1),
      hr(),
      h4(textOutput("variance_text")),
      h4(textOutput("success_text"))
    ),
    
    mainPanel(
      plotOutput("pcaPlot", height = "500px"),
      plotOutput("varPlot", height = "200px")
    )
  )
)

# 4. Define Server Logic
server <- function(input, output) {
  
  # Reactive calculations for the projections
  projection_data <- reactive({
    theta <- input$angle * pi / 180
    
    # Calculate scalar distance of each point projected onto the line vector (cos, sin)
    proj_scalar <- df$x * cos(theta) + df$y * sin(theta)
    
    # Map those scalars back to x,y coordinates to draw the points on the line
    data.frame(
      x_orig = df$x,
      y_orig = df$y,
      x_proj = proj_scalar * cos(theta),
      y_proj = proj_scalar * sin(theta),
      scalar = proj_scalar
    )
  })
  
  # Check if student found the PC1 (within a 1-degree margin of error)
  found_pc1 <- reactive({
    abs(input$angle - pc1_angle_deg) <= 1
  })
  
  # Main interactive scatterplot
  output$pcaPlot <- renderPlot({
    p_data <- projection_data()
    theta <- input$angle * pi / 180
    is_pc1 <- found_pc1()
    
    # Dynamic styling if they find the Principal Component
    line_color <- ifelse(is_pc1, "#27ae60", "#2c3e50") # Green if found, dark grey if searching
    line_size <- ifelse(is_pc1, 2, 1)
    
    ggplot(p_data) +
      # Draw the projection dashed lines from original points to the rotating line
      geom_segment(aes(x = x_orig, y = y_orig, xend = x_proj, yend = y_proj), 
                   linetype = "dashed", color = "gray80", alpha = 0.6) +
      # Draw the original data cloud
      geom_point(aes(x = x_orig, y = y_orig), color = "gray50", size = 2, alpha = 0.5) +
      # Draw the rotating line (extended out to cover the plot area)
      geom_segment(aes(x = -15 * cos(theta), y = -15 * sin(theta), 
                       xend = 15 * cos(theta), yend = 15 * sin(theta)), 
                   color = line_color, size = line_size) +
      # Draw the projected points sitting on the line
      geom_point(aes(x = x_proj, y = y_proj), color = "#e74c3c", size = 3) +
      
      # Orthogonal line (PC2) appears automatically if PC1 is found
      {if(is_pc1) geom_segment(aes(x = -15 * cos(theta + pi/2), y = -15 * sin(theta + pi/2), 
                                   xend = 15 * cos(theta + pi/2), yend = 15 * sin(theta + pi/2)), 
                               color = "#8e44ad", size = 1.5, linetype = "dashed")} +
      
      coord_fixed(xlim = c(-10, 10), ylim = c(-10, 10)) +
      theme_minimal() +
      labs(title = "Data Cloud & Projected Variance", x = "Variable X", y = "Variable Y") +
      theme(text = element_text(size = 14))
  })
  
  # Secondary plot mapping the variance live
  output$varPlot <- renderPlot({
    current_var <- var(projection_data()$scalar)
    is_pc1 <- found_pc1()
    bar_color <- ifelse(is_pc1, "#27ae60", "#e74c3c")
    
    df_bar <- data.frame(
      Category = c("Current Variance", "Max Possible (PC1)"),
      Variance = c(current_var, max_variance)
    )
    
    ggplot(df_bar, aes(x = Category, y = Variance, fill = Category)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c(bar_color, "#bdc3c7")) +
      coord_flip() +
      theme_minimal() +
      ylim(0, max_variance * 1.1) +
      theme(legend.position = "none", text = element_text(size = 14)) +
      labs(x = "", y = "Variance of Red Projected Points")
  })
  
  # Text feedback
  output$variance_text <- renderText({
    paste("Current Variance:", round(var(projection_data()$scalar), 2))
  })
  
  output$success_text <- renderText({
    if(found_pc1()) "Principal Component 1 Discovered! PC2 drawn orthogonally." else "Keep rotating..."
  })
}

shinyApp(ui = ui, server = server)