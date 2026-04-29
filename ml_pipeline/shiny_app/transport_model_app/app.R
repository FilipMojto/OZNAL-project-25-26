library(shiny)
library(tidyverse)
library(glmnet)
library(rpart)
library(ranger)

source("R/utils.R")

# default_data <- readRDS("../../datasets/training_ready_data.rds")

ui <- fluidPage(
  titlePanel("Regression Model Comparison"),
  
  tags$style(HTML("
    #predictors {
      column-count: 2;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload dataset (.rds or .csv)"),
      
      selectInput("model", "Choose model:",
                  choices = c("Linear Regression", "Polynomial Regression",
                              "Lasso", "Elastic Net", "DT", "RF")),
      
      selectInput("target", "Target variable:", choices = NULL),
      
      checkboxGroupInput(
        "predictors",
        "Predictors:",
        choices = NULL,
        selected = NULL,
        inline = FALSE
      ),
      
      sliderInput("train_split", "Training split:",
                  min = 0.5, max = 0.9, value = 0.8),
      
      conditionalPanel(
        condition = "input.model == 'Polynomial Regression'",
        numericInput("degree", "Polynomial degree:", value = 2, min = 1, max = 5)
      ),
      
      conditionalPanel(
        condition = "input.model == 'Elastic Net'",
        sliderInput("alpha", "Elastic Net alpha:", min = 0, max = 1, value = 0.5)
      ),
      
      conditionalPanel(
        condition = "input.model == 'Lasso' || input.model == 'Elastic Net'",
        sliderInput("lambda", "Lambda:", min = 0.0001, max = 1, value = 0.01)
      ),
      
      conditionalPanel(
        condition = "input.model == 'DT'",
        sliderInput("dt_maxdepth", "DT max depth:", min = 1, max = 20, value = 6),
        sliderInput("dt_cp", "DT complexity parameter:", min = 0.0001, max = 0.05,
                    value = 0.001)
      ),
      
      conditionalPanel(
        condition = "input.model == 'RF'",
        sliderInput("rf_trees", "Number of trees:", min = 50, max = 500,
                    value = 200, step = 50),
        sliderInput("rf_mtry", "mtry:", min = 1, max = 30, value = 6),
        sliderInput("rf_min_node", "Min node size:", min = 1, max = 50, value = 10)
      ),
      
      actionButton("fit_model", "Fit / Update model", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", tableOutput("data_preview")),
        tabPanel("Predicted vs Actual", plotOutput("pred_actual")),
        tabPanel("Residuals", plotOutput("residuals")),
        tabPanel("Scale-Location", plotOutput("scale_location")),
        tabPanel("QQ Plot", plotOutput("qq_plot")),
        tabPanel("Metrics", tableOutput("metrics")),
        tabPanel("Selected Features", tableOutput("selected_features"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    
    path <- input$file$datapath
    
    if (stringr::str_ends(input$file$name, ".rds")) {
      readRDS(path)
    } else {
      read.csv(path)
    }
  })
  
  observe({
    req(dataset())
    data <- dataset()
    
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    updateSelectInput(
      session,
      "target",
      choices = numeric_cols,
      selected = if ("segment_time_s" %in% numeric_cols) "segment_time_s" else numeric_cols[1]
    )
  })
  
  observeEvent(input$target, {
    req(dataset())
    
    data <- dataset()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    predictor_choices <- setdiff(numeric_cols, input$target)
    
    updateCheckboxGroupInput(
      session,
      "predictors",
      choices = predictor_choices,
      selected = predictor_choices
    )
  })
  
  fit <- eventReactive(input$fit_model, {
    req(dataset())
    req(input$target)
    req(input$predictors)
    
    validate(
      need(length(input$predictors) > 0, "Select at least one predictor.")
    )
    
    
    fit_selected_model(
      data = dataset(),
      model_type = input$model,
      target = input$target,
      predictors = input$predictors,
      split = input$train_split,
      alpha = input$alpha,
      lambda = input$lambda,
      degree = input$degree,
      dt_maxdepth = input$dt_maxdepth,
      dt_cp = input$dt_cp,
      rf_trees = input$rf_trees,
      rf_mtry = input$rf_mtry,
      rf_min_node = input$rf_min_node
    )
  })
  
  # output$data_preview <- renderTable({
  #   req(dataset())
  #   head(dataset(), 10)
  # })
  
  output$data_preview <- renderTable({
    validate(
      need(!is.null(input$file), "Please upload a .rds or .csv dataset first.")
    )
    
    head(dataset(), 10)
  })
  
  output$pred_actual <- renderPlot({
    req(fit())
    fit_obj <- fit()
    
    plot_predicted_vs_actual(
      actual = fit_obj$test_actual,
      predicted = fit_obj$pred_test,
      title = paste(input$model, ": Predicted vs Actual")
    )
  })
  
  output$residuals <- renderPlot({
    req(fit())
    fit_obj <- fit()
    
    plot_residuals(
      actual = fit_obj$test_actual,
      predicted = fit_obj$pred_test,
      title = paste(input$model, ": Residuals")
    )
  })
  
  output$scale_location <- renderPlot({
    req(fit())
    fit_obj <- fit()
    
    residuals <- fit_obj$test_actual - fit_obj$pred_test
    
    plot_scale_location(
      fitted = fit_obj$pred_test,
      residuals = residuals,
      title = paste(input$model, ": Scale-Location Plot")
    )
  })
  
  output$qq_plot <- renderPlot({
    req(fit())
    fit_obj <- fit()
    
    residuals <- fit_obj$test_actual - fit_obj$pred_test
    
    plot_qq(
      residuals,
      title = paste(input$model, ": QQ Plot")
    )
  })
  
  output$metrics <- renderTable({
    req(fit())
    fit()$metrics
  })
  
  output$selected_features <- renderTable({
    req(fit())
    tibble(
      selected_feature = fit()$selected_features
    )
  })
}

shinyApp(ui, server)