library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Logistic Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Dataset (CSV)", accept = ".csv"),
      textInput("response", "Response Variable (y)", value = "y"),
      textInput("predictors", "Predictor Variables (comma-separated)", value = "x1,x2,x3"),
      numericInput("n_bootstrap", "Number of Bootstrap Samples", value = 100, min = 1),
      numericInput("alpha", "Confidence Interval Alpha Level", value = 0.05, min = 0, max = 1),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      h4("Estimated Beta Coefficients"),
      verbatimTextOutput("beta_output"),
      h4("Bootstrap Confidence Intervals"),
      tableOutput("bootstrap_ci"),
      h4("Confusion Matrix Metrics"),
      tableOutput("confusion_metrics"),
      plotOutput("bootstrap_plot")
    )
  )
)

# Define Server
server <- function(input, output) {
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  predictors_matrix <- reactive({
    req(data(), input$predictors)
    as.matrix(data()[, strsplit(input$predictors, ",")[[1]]])
  })
  
  response_vector <- reactive({
    req(data(), input$response)
    as.numeric(data()[[input$response]])
  })
  
  beta_estimation <- eventReactive(input$run_analysis, {
    X <- predictors_matrix()
    y <- response_vector()
    opt_beta_est(X, y)
  })
  
  bootstrap_results <- eventReactive(input$run_analysis, {
    X <- predictors_matrix()
    y <- response_vector()
    bootstrap_ci(X, y, opt_beta_est, alpha = input$alpha, n_bootstrap = input$n_bootstrap)
  })
  
  confusion_matrix <- eventReactive(input$run_analysis, {
    X <- predictors_matrix()
    y <- response_vector()
    beta <- beta_estimation()
    y_pred <- ifelse(1 / (1 + exp(-cbind(1, X) %*% beta)) > 0.5, 1, 0)
    confusion_metrics(y, y_pred)
  })
  
  output$beta_output <- renderPrint({
    beta_estimation()
  })
  
  output$bootstrap_ci <- renderTable({
    bootstrap_results()
  })
  
  output$confusion_metrics <- renderTable({
    metrics <- confusion_matrix()
    data.frame(
      Metric = names(metrics),
      Value = unlist(metrics)
    )
  })
  
  output$bootstrap_plot <- renderPlot({
    ci <- bootstrap_results()
    ggplot(ci, aes(x = Coefficient, ymin = Lower_CI, ymax = Upper_CI)) +
      geom_errorbar(width = 0.2) +
      geom_point(aes(y = (Lower_CI + Upper_CI) / 2), size = 2) +
      labs(title = "Bootstrap Confidence Intervals",
           x = "Coefficient Index",
           y = "Value") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
