
# ---- ALTERNATIVE (LESS VISUALLY APPEALING) ----
# plot_homoscedasticity_plot <- function(fitted, residuals){
#   data.frame(
#     fitted = fitted,
#     residuals = residuals
#   ) %>%
#     ggplot(aes(x = fitted, y = residuals)) +
#     geom_point(alpha = 0.2) +
#     geom_hline(yintercept = 0, color = "red") +
#     theme_minimal()
# }
plot_homoscedasticity_plot <- function(fitted, residuals){
  data.frame(
    fitted = fitted,
    residuals = residuals
  ) %>%
    ggplot(aes(x = fitted, y = residuals)) +
    
    # points
    geom_point(alpha = 0.1, size = 0.6, color = "steelblue") +
    
    # horizontal zero line
    geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
    
    # smooth trend (VERY IMPORTANT)
    geom_smooth(
      method = "gam",
      formula = y ~ s(x, k = 5),
      color = "orange",
      se = FALSE,
      linewidth = 1
    ) +
    
    # optional: highlight spread
    stat_density_2d(alpha = 0.2, color = "grey50") +
    
    theme_minimal() +
    labs(
      title = "Residuals vs Fitted Values",
      subtitle = "Checking homoscedasticity",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

plot_scale_location <- function(fitted, residuals){
  data.frame(
    fitted = fitted,
    sqrt_abs_resid = sqrt(abs(residuals))
  ) %>%
    ggplot(aes(x = fitted, y = sqrt_abs_resid)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "gam", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(
      title = "Scale-Location Plot",
      y = "√|Residuals|"
    )
}

# --- ALTERNATIVE TO BINNED RELATIONSHIP PLOT ---
# plot_scatterplot_line <- function(data, x, y = "segment_time_s") {
#   ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
#     geom_point(alpha = 0.08, size = 0.6) +
#     geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.8) +
#     geom_smooth(
#       method = "gam",
#       formula = y ~ s(x, k = 5),
#       color = "blue",
#       se = FALSE,
#       linewidth = 0.9
#     ) +
#     theme_minimal() +
#     labs(
#       title = x,
#       x = x,
#       y = y
#     ) +
#     theme(
#       plot.title = element_text(size = 10, face = "bold"),
#       axis.text = element_text(size = 8),
#       axis.title = element_text(size = 8)
#     )
# }
# scatterplot_list <- purrr::map(
#   features_to_check,
#   ~ plot_scatterplot_line(sample_data, .x)
# )
# 
# combined_plot <- patchwork::wrap_plots(scatterplot_list, ncol = 2)
# 
# combined_plot

plot_binned_relationship <- function(data, x, y = "segment_time_s", bins = 30, 
                                     label_size = 12, text_size = 10) {
  data %>%
    mutate(bin = ntile(.data[[x]], bins)) %>%
    group_by(bin) %>%
    summarise(
      x_mean = mean(.data[[x]], na.rm = TRUE),
      y_mean = mean(.data[[y]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = x_mean, y = y_mean)) +
    geom_point(size = 1.2, alpha = 0.4) +
    geom_smooth(aes(color = "Linear"), method = "lm", se = FALSE) +
    geom_smooth(
      aes(color = "Non-linear"),
      method = "gam",
      formula = y ~ s(x, k = 5),
      se = FALSE
    ) +
    scale_color_manual(
      name = "Legend", # This name must be identical across all plots
      values = c("Linear" = "red", "Non-linear" = "blue")
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = label_size),
      axis.text = element_text(size = text_size)
    ) +
    labs(x = x, y = paste("Mean", y))
}

# ---- Evaluation ---- #

evaluate_regression <- function(actual, predicted, round_to = NULL, model_name = "model") {
  if (!is.null(round_to)) {
    predicted <- round(predicted / round_to) * round_to
  }
  
  rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  mae  <- mean(abs(actual - predicted), na.rm = TRUE)
  
  ss_total <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  ss_res   <- sum((actual - predicted)^2, na.rm = TRUE)
  r2 <- 1 - (ss_res / ss_total)
  
  tibble::tibble(
    model = model_name,
    RMSE = rmse,
    MAE = mae,
    R2 = r2
  )
}

# explicitly used in cv
eval_metrics <- function(actual, predicted) {
  tibble(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    R2 = cor(actual, predicted)^2
  )
}

# used for LR, PR, DT, RF
eval_train_test <- function(model, model_label, train_data, test_data, predict_fun) {
  pred_train <- predict_fun(model, train_data)
  pred_test  <- predict_fun(model, test_data)
  
  dplyr::bind_rows(
    evaluate_regression(
      actual = train_data$segment_time_s,
      predicted = pred_train,
      model_name = paste0(model_label, " train")
    ),
    evaluate_regression(
      actual = test_data$segment_time_s,
      predicted = pred_test,
      model_name = paste0(model_label, " test")
    )
  )
}

# used for LASSO/ElASTIC
eval_train_test_glmnet <- function(model, model_label, x_train, y_train, x_test, y_test) {
  pred_train <- as.vector(predict(model, newx = x_train))
  pred_test  <- as.vector(predict(model, newx = x_test))
  
  dplyr::bind_rows(
    evaluate_regression(
      actual = y_train,
      predicted = pred_train,
      model_name = paste0(model_label, " train")
    ),
    evaluate_regression(
      actual = y_test,
      predicted = pred_test,
      model_name = paste0(model_label, " test")
    )
  )
}

plot_predicted_vs_actual <- function(
    data,
    predictions,
    target = "segment_time_s",
    title = "Predicted vs Actual Values",
    add_smooth = TRUE
) {
  plot_data <- data.frame(
    actual = data[[target]],
    predicted = predictions
  )
  
  # Create perfect line data
  line_data <- data.frame(
    x = range(plot_data$actual)
  )
  line_data$y <- line_data$x
  
  p <- ggplot(plot_data, aes(x = actual, y = predicted)) +
    
    geom_point(alpha = 0.25, size = 1, color = "steelblue") +
    
    # Perfect prediction line (now mapped → legend works)
    geom_line(
      data = line_data,
      aes(x = x, y = y, color = "Perfect prediction"),
      linewidth = 1,
      linetype = "dashed"
    )
  
  if (add_smooth) {
    p <- p + geom_smooth(
      aes(color = "Model trend"),
      method = "gam",
      formula = y ~ s(x, k = 5),
      se = FALSE,
      linewidth = 1
    )
  }
  
  p +
    scale_color_manual(
      name = "Lines",
      values = c(
        "Perfect prediction" = "red",
        "Model trend" = "darkorange"
      )
    ) +
    coord_equal() +
    theme_minimal() +
    labs(
      title = title,
      subtitle = "Comparison of predictions vs actual values",
      x = "Actual value",
      y = "Predicted value"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# ---- Feature Selection ---- #

select_model_features <- function(data, features = NULL, target = "segment_time_s") {
  
  # If NULL or empty → return full dataset
  if (is.null(features) || length(features) == 0) {
    return(data)
  }
  
  keep_cols <- unique(c(target, features))
  
  missing_cols <- setdiff(keep_cols, names(data))
  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  data %>%
    dplyr::select(dplyr::any_of(keep_cols))
}

# ---- ENCODING ----- #

fit_frequency_encoding <- function(train_df, col) {
  freq_col <- paste0(col, "_freq")
  # freq_col <- paste0(col)
  
  freq_table <- train_df %>%
    dplyr::count(.data[[col]])
  
  names(freq_table)[names(freq_table) == "n"] <- freq_col
  
  freq_table[[freq_col]] <- freq_table[[freq_col]] / nrow(train_df)
  
  freq_table
}

apply_frequency_encoding <- function(df, freq_table, col) {
  freq_col <- paste0(col, "_freq")
  # freq_col <- paste0(col)
  
  
  df %>%
    dplyr::left_join(freq_table, by = col) %>%
    dplyr::mutate(
      !!freq_col := ifelse(is.na(.data[[freq_col]]), 0, .data[[freq_col]])
    ) %>%
    dplyr::select(-dplyr::all_of(col))
}


# ---- Lasso & Elastic Helpers ---- #

prepare_glmnet_data <- function(train_data, test_data, target) {
  feature_names <- setdiff(names(train_data), target)
  
  x_train <- as.matrix(train_data[, feature_names])
  y_train <- train_data[[target]]
  
  x_test <- as.matrix(test_data[, feature_names])
  y_test <- test_data[[target]]
  
  list(
    x_train = x_train,
    y_train = y_train,
    x_test = x_test,
    y_test = y_test,
    feature_names = feature_names
  )
}

# ---- VIF Helpers (LR, PR) ----

format_vif <- function(vif_values) {
  data.frame(
    feature = names(vif_values),
    vif = as.numeric(vif_values)
  ) %>%
    dplyr::arrange(desc(vif))
}

evaluate_regression <- function(actual, predicted, dataset_name = "test", split = 0.8, seed = 42) {
  tibble(
    dataset = dataset_name,
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )
}

create_train_test_split <- function(data, predictors, split = 0.8, seed = 42) {
  set.seed(seed)
  
  # 1. Vyber 80% trip_id (nie riadkov)
  unique_trips <- unique(data$trip_id)
  train_trips <- sample(unique_trips, size = split * length(unique_trips))
  
  # 2. Filter podla trip_id - cely trip ide bud do trainu, alebo do testu
  train_data <- data %>%
    filter(trip_id %in% train_trips)
  
  test_data <- data %>%
    filter(!trip_id %in% train_trips)
  
  # 3. Sanity check - ziadny trip nesmie byt v oboch
  shared_trips <- intersect(unique(train_data$trip_id), unique(test_data$trip_id))
  stopifnot(length(shared_trips) == 0)
  
  cat("Train trips:", length(unique(train_data$trip_id)), 
      "| Test trips:", length(unique(test_data$trip_id)), "\n")
  cat("Train rows:", nrow(train_data), 
      "| Test rows:", nrow(test_data), "\n")
  
  # 4. Az teraz dropnut trip_id - v modeli ho nechces
  train_data <- train_data %>% select(-trip_id)
  test_data  <- test_data  %>% select(-trip_id)
  
  freq_cols <- c(
    "route_short_name",
    "route_id",
    "trip_headsign",
    "stop_name",
    "stop_code"
  )
  
  # glimpse(train_data)
  
  for (col in freq_cols) {
    freq_table <- fit_frequency_encoding(train_data, col)
    
    train_data <- apply_frequency_encoding(train_data, freq_table, col)
    test_data  <- apply_frequency_encoding(test_data, freq_table, col)
  }
  
  predictors <- ifelse(
    predictors %in% freq_cols,
    paste0(predictors, "_freq"),
    predictors
  )
  
  list(
    train = train_data,
    test = test_data,
    predictors = predictors
  )
}


fit_selected_model <- function(data, model_type, target, predictors,
                               split = 0.8,
                               alpha = 0.5,
                               lambda = 0.01,
                               degrees = NULL,
                               dt_maxdepth = 6,
                               dt_cp = 0.001,
                               rf_trees = 200,
                               rf_mtry = 6,
                               rf_min_node = 10) {
  
  split_data <- create_train_test_split(data = data, predictors = predictors, split = split, seed = 42)
  
  train_data <- split_data$train
  test_data <- split_data$test
  predictors <- split_data$predictors
  
  formula_base <- as.formula(paste(target, "~ ."))
  
  if (model_type == "Linear Regression") {
    
    model <- lm(formula_base, data = train_data)
    model <- step(model, direction = "backward", trace = FALSE)
    
    pred_train <- predict(model, newdata = train_data)
    pred_test <- predict(model, newdata = test_data)
    
    selected_features <- attr(terms(model), "term.labels")
  }
  
  else if (model_type == "Polynomial Regression") {

    if (is.null(degrees)) {
      degrees <- setNames(rep(2L, length(predictors)), predictors)
    }

    # Remap degree keys for freq-encoded columns
    freq_cols <- c("route_short_name", "route_id", "trip_headsign", "stop_name", "stop_code")
    names(degrees) <- ifelse(names(degrees) %in% freq_cols,
                             paste0(names(degrees), "_freq"),
                             names(degrees))

    poly_features  <- intersect(names(degrees)[degrees > 1], predictors)
    linear_features <- setdiff(predictors, poly_features)

    terms_list <- c(
      linear_features,
      sapply(poly_features, function(f) paste0("poly(", f, ", ", degrees[[f]], ")"))
    )

    formula_poly <- as.formula(paste(target, "~", paste(terms_list, collapse = " + ")))

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

# plot_predicted_vs_actual <- function(actual, predicted, title = "Predicted vs Actual") {
#   tibble(
#     actual = actual,
#     predicted = predicted
#   ) %>%
#     ggplot(aes(x = actual, y = predicted)) +
#     geom_point(alpha = 0.4) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     theme_minimal() +
#     labs(
#       title = title,
#       x = "Actual values",
#       y = "Predicted values"
#     )
# }

plot_predicted_vs_actual <- function(
    actual,
    predicted,
    target = "segment_time_s",
    title = "Predicted vs Actual Values",
    add_smooth = TRUE
) {
  plot_data <- data.frame(
    actual = actual,
    predicted = predicted
  )
  
  # Create perfect line data
  line_data <- data.frame(
    x = range(plot_data$actual)
  )
  line_data$y <- line_data$x
  
  p <- ggplot(plot_data, aes(x = actual, y = predicted)) +
    
    geom_point(alpha = 0.25, size = 1, color = "steelblue") +
    
    # Perfect prediction line (now mapped → legend works)
    geom_line(
      data = line_data,
      aes(x = x, y = y, color = "Perfect prediction"),
      linewidth = 1,
      linetype = "dashed"
    )
  
  if (add_smooth) {
    p <- p + geom_smooth(
      aes(color = "Model trend"),
      method = "gam",
      formula = y ~ s(x, k = 5),
      se = FALSE,
      linewidth = 1
    )
  }
  
  p +
    scale_color_manual(
      name = "Lines",
      values = c(
        "Perfect prediction" = "red",
        "Model trend" = "darkorange"
      )
    ) +
    coord_equal() +
    theme_minimal() +
    labs(
      title = title,
      subtitle = "Comparison of predictions vs actual values",
      x = "Actual value",
      y = "Predicted value"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
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

# checking homoscedasticity
plot_scale_location <- function(fitted, residuals, title = "Scale-Location Plot") {
  data.frame(
    fitted = fitted,
    sqrt_abs_resid = sqrt(abs(residuals))
  ) %>%
    ggplot(aes(x = fitted, y = sqrt_abs_resid)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "gam", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(
      title = title,
      x = "Fitted values",
      y = "√|Residuals|"
    )
}

plot_qq <- function(residuals, title = "QQ Plot") {
  data.frame(residuals = residuals) %>%
    ggplot(aes(sample = residuals)) +
    stat_qq(alpha = 0.3) +
    stat_qq_line(color = "red") +
    theme_minimal() +
    labs(
      title = title,
      subtitle = "Check of residual normality"
    )
}
