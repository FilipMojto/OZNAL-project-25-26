library(shiny)
library(tidyverse)
library(glmnet)
library(rpart)
library(ranger)
library(shinycssloaders)

source("R/utils.R")

# default_data <- readRDS("../../datasets/training_ready_data.rds")

ui <- fluidPage(
  titlePanel("Regression Model Comparison"),
  
  tags$style(HTML("
    .pred-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 0 8px; }
    .pred-row { display: flex; align-items: flex-start; gap: 4px; min-width: 0; }
    .pred-row .form-group { margin-bottom: 0; flex: 1; min-width: 0; }
    .pred-row .checkbox { margin-top: 2px; margin-bottom: 2px; }
    .pred-row .checkbox label { font-size: 12px; font-weight: normal; word-break: break-all; }
    .pred-row input[type=number] { width: 46px !important; padding: 2px 4px; height: 24px; font-size: 12px; flex-shrink: 0; margin-top: 3px; }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload dataset (.rds or .csv)"),
      
      selectInput("model", "Choose model:",
                  choices = c("Linear Regression", "Polynomial Regression",
                              "Lasso", "Elastic Net", "DT", "RF")),
      
      uiOutput("predictors_ui"),
      
      sliderInput("train_split", "Training split:",
                  min = 0.5, max = 0.9, value = 0.8),
      
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
        tabPanel("Data Preview", withSpinner(tableOutput("data_preview"))),
        tabPanel("Predicted vs Actual", withSpinner(plotOutput("pred_actual", height = "600px"))),
        tabPanel("Residuals", withSpinner(plotOutput("residuals"))),
        tabPanel("Scale-Location", withSpinner(plotOutput("scale_location"))),
        tabPanel("QQ Plot", withSpinner(plotOutput("qq_plot"))),
        tabPanel("Metrics", withSpinner(tableOutput("metrics"))),
        tabPanel("Selected Features", withSpinner(tableOutput("selected_features")))
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
  
  predictor_choices_r <- reactive({
    req(dataset())
    data <- dataset()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    setdiff(numeric_cols, "segment_time_s")
  })

  poly_degree_defaults <- c(
    stop_lat                  = 2,
    stop_lon                  = 2,
    distance_sin_interaction  = 2,
    distance_cos_interaction  = 2
  )

  output$predictors_ui <- renderUI({
    choices <- predictor_choices_r()
    is_poly <- isTRUE(input$model == "Polynomial Regression")

    tagList(
      tags$label("Predictors:"),
      tags$div(class = "pred-grid",
        lapply(choices, function(p) {
          if (is_poly) {
            default_deg <- if (p %in% names(poly_degree_defaults)) poly_degree_defaults[[p]] else 1
            tags$div(class = "pred-row",
              checkboxInput(paste0("pred_", p), label = p, value = TRUE),
              numericInput(paste0("deg_", p), label = NULL, value = default_deg, min = 1, max = 10, width = "52px")
            )
          } else {
            tags$div(class = "pred-row",
              checkboxInput(paste0("pred_", p), label = p, value = TRUE)
            )
          }
        })
      )
    )
  })

  selected_predictors <- reactive({
    choices <- predictor_choices_r()
    Filter(function(p) isTRUE(input[[paste0("pred_", p)]]), choices)
  })
  
  fit <- eventReactive(input$fit_model, {
    req(dataset())
    preds <- selected_predictors()

    validate(
      need(length(preds) > 0, "Select at least one predictor.")
    )

    degrees <- if (input$model == "Polynomial Regression") {
      setNames(
        sapply(preds, function(p) {
          val <- input[[paste0("deg_", p)]]
          if (is.null(val) || is.na(val)) 1L else max(1L, as.integer(val))
        }),
        preds
      )
    } else {
      NULL
    }

    fit_selected_model(
      data = dataset(),
      model_type = input$model,
      target = "segment_time_s",
      predictors = preds,
      split = input$train_split,
      alpha = input$alpha,
      lambda = input$lambda,
      degrees = degrees,
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