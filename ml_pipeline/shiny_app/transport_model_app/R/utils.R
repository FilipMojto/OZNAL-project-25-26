evaluate_regression <- function(actual, predicted, dataset_name = "test") {
  tibble(
    dataset = dataset_name,
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )
}

create_train_test_split <- function(data, target, predictors, split = 0.8, seed = 42) {
  set.seed(seed)
  
  model_data <- data %>%
    select(all_of(c(target, predictors))) %>%
    drop_na()
  
  train_index <- sample(seq_len(nrow(model_data)), size = floor(split * nrow(model_data)))
  
  list(
    train = model_data[train_index, ],
    test = model_data[-train_index, ]
  )
}

fit_selected_model <- function(data, model_type, target, predictors,
                               split = 0.8,
                               alpha = 0.5,
                               lambda = 0.01,
                               degree = 2,
                               dt_maxdepth = 6,
                               dt_cp = 0.001,
                               rf_trees = 200,
                               rf_mtry = 6,
                               rf_min_node = 10) {
  
  split_data <- create_train_test_split(data, target, predictors, split)
  
  train_data <- split_data$train
  test_data <- split_data$test
  
  formula_base <- as.formula(paste(target, "~ ."))
  
  if (model_type == "Linear Regression") {
    
    model <- lm(formula_base, data = train_data)
    model <- step(model, direction = "backward", trace = FALSE)
    
    pred_train <- predict(model, newdata = train_data)
    pred_test <- predict(model, newdata = test_data)
    
    selected_features <- attr(terms(model), "term.labels")
  }
  
  else if (model_type == "Polynomial Regression") {
    
    poly_terms <- paste0("poly(", predictors, ", ", degree, ", raw = TRUE)", collapse = " + ")
    formula_poly <- as.formula(paste(target, "~", poly_terms))
    
    model <- lm(formula_poly, data = train_data)
    model <- step(model, direction = "backward", trace = FALSE)
    
    pred_train <- predict(model, newdata = train_data)
    pred_test <- predict(model, newdata = test_data)
    
    selected_features <- attr(terms(model), "term.labels")
  }
  
  else if (model_type == "Lasso") {
    
    x_train <- as.matrix(train_data[, predictors])
    y_train <- train_data[[target]]
    x_test <- as.matrix(test_data[, predictors])
    
    model <- glmnet(
      x = x_train,
      y = y_train,
      alpha = 1,
      lambda = lambda,
      standardize = TRUE
    )
    
    pred_train <- as.vector(predict(model, newx = x_train))
    pred_test <- as.vector(predict(model, newx = x_test))
    
    coef_values <- coef(model)
    selected_features <- rownames(coef_values)[coef_values[, 1] != 0]
    selected_features <- selected_features[selected_features != "(Intercept)"]
  }
  
  else if (model_type == "Elastic Net") {
    
    x_train <- as.matrix(train_data[, predictors])
    y_train <- train_data[[target]]
    x_test <- as.matrix(test_data[, predictors])
    
    model <- glmnet(
      x = x_train,
      y = y_train,
      alpha = alpha,
      lambda = lambda,
      standardize = TRUE
    )
    
    pred_train <- as.vector(predict(model, newx = x_train))
    pred_test <- as.vector(predict(model, newx = x_test))
    
    coef_values <- coef(model)
    selected_features <- rownames(coef_values)[coef_values[, 1] != 0]
    selected_features <- selected_features[selected_features != "(Intercept)"]
  }
  
  else if (model_type == "DT") {
    
    model <- rpart(
      formula_base,
      data = train_data,
      method = "anova",
      control = rpart.control(
        maxdepth = dt_maxdepth,
        cp = dt_cp
      )
    )
    
    pred_train <- predict(model, newdata = train_data)
    pred_test <- predict(model, newdata = test_data)
    
    selected_features <- names(model$variable.importance)
  }
  
  else if (model_type == "RF") {
    
    safe_mtry <- min(rf_mtry, length(predictors))
    
    model <- ranger(
      formula_base,
      data = train_data,
      num.trees = rf_trees,
      mtry = safe_mtry,
      min.node.size = rf_min_node,
      importance = "impurity",
      seed = 42
    )
    
    pred_train <- predict(model, data = train_data)$predictions
    pred_test <- predict(model, data = test_data)$predictions
    
    selected_features <- names(sort(model$variable.importance, decreasing = TRUE))
  }
  
  metrics <- bind_rows(
    evaluate_regression(train_data[[target]], pred_train, "train"),
    evaluate_regression(test_data[[target]], pred_test, "test")
  )
  
  list(
    model = model,
    train_data = train_data,
    test_data = test_data,
    train_actual = train_data[[target]],
    test_actual = test_data[[target]],
    pred_train = pred_train,
    pred_test = pred_test,
    metrics = metrics,
    selected_features = selected_features
  )
}

plot_predicted_vs_actual <- function(actual, predicted, title = "Predicted vs Actual") {
  tibble(
    actual = actual,
    predicted = predicted
  ) %>%
    ggplot(aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = title,
      x = "Actual values",
      y = "Predicted values"
    )
}

plot_residuals <- function(actual, predicted, title = "Residuals") {
  tibble(
    predicted = predicted,
    residual = actual - predicted
  ) %>%
    ggplot(aes(x = predicted, y = residual)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = title,
      x = "Predicted values",
      y = "Residuals"
    )
}