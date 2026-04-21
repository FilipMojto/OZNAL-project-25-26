# libraries
library("tidyverse")
library("readr")
library("ggplot2")
library("patchwork")
library("dplyr")
library("purrr")

# =========================
# Load data
# =========================
timetable <- read_csv("../data/interim/dataset.csv")
# table(timetable$day_type)

# =========================
# Restrict dataset for testing purposes
# =========================

# timetable <- timetable %>% head(n=10000)

timetable <- timetable %>%
  group_by(day_type) %>%  # important variable
  sample_frac(0.1) %>%   # 10% sample
  ungroup()

# =========================
# Basic info
# =========================
dim(timetable)
glimpse(timetable)


# =========================
# 1. Missing values
# =========================

missing_summary <- timetable %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count") %>%
  mutate(missing_pct = missing_count / nrow(timetable) * 100) %>%
  arrange(desc(missing_pct))

# ===
# 1.1 Low_missing features (can be kept - must be imputed)
# ===

low_missing <- missing_summary %>%
  filter(missing_pct > 0 & missing_pct < 50)

low_missing

# ===
# 1.2 High missing features (must be removed)
# ===

high_missing <- missing_summary %>%
  filter(missing_pct >= 50) %>%
  pull(column)

high_missing

# =========================
# 2. Constant & Near-Constant features
# =========================
variance_summary <- timetable %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "n_unique") %>%
  arrange(n_unique)
variance_summary

# ===
# 2.1 Constant features (should be dropped immediately)
# ===

constant_features <- variance_summary %>%
  filter(n_unique == 1) %>% pull(column)

constant_features

# ===
# 2.2 Near-zero variance (should be dropped too, too weak predictors)
# ===


# formula: ratio = freq_1 / freq_2
nzv_summary <- timetable %>%
  summarise(across(everything(), ~ {
    freq <- sort(table(.), decreasing = TRUE)
    if (length(freq) <= 1) return(NA)
    freq[1] / freq[2]
  })) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "freq_ratio") %>%
  arrange(desc(freq_ratio))

print(nzv_summary)

# Threshold example: freq_ratio > 20
nzv_cols <- nzv_summary %>%
  filter(freq_ratio > 20) %>%
  pull(column)

print(nzv_cols)

# =========================
# 4. Handling duplicates
# =========================

# ===
# 4.1 Duplicate rows (should be removed)
# ===

dup_row_indexes <- timetable %>%
  duplicated() %>%
  which()

print(paste("Duplicate rows:", dup_row_indexes))

# ===
# 4.2 Duplicate cols (should be removed)
# ===

find_duplicate_columns <- function(df) {
  dup_col_indexes <- which(duplicated(as.list(df)))
  dup_cols <- names(df)[dup_col_indexes]
  
  return(dup_cols)
}

# More efficient check for duplicate columns
# R looks at the "top level" of the data frame. duplicated() now looks at each 
# vector (column) as a single item in a list and compares them.
# dup_col_indexes <- which(duplicated(as.list(timetable)))
# dup_cols <- names(timetable)[dup_col_indexes]
dup_cols <- find_duplicate_columns(timetable)

paste("Duplicate cols")
dup_cols

# =========================
# 5. High cardinality categorical variables
# =========================
cat_summary <- timetable %>%
  select(where(is.character)) %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "n_unique") %>%
  arrange(desc(n_unique))

print(cat_summary)

# ===
# Columns with too many categories (characters)
# ===
high_cardinality <- cat_summary %>%
  filter(n_unique > 10)
high_cardinality

high_cardinality_cols <- high_cardinality %>% pull(column)
high_cardinality_cols

# =========================
# 6. Correlation (numeric only)
# =========================

# here we drop the feature from a highly correlated pair that has
# smaller correlation with target
# target_cor <- timetable %>%
#   select(where(is.numeric)) %>%
#   cor(use = "complete.obs")
# 
# target_cor_vec <- abs(target_cor[, "segment_time_s"])
# 
# cor_pairs <- cor_pairs %>%
#   mutate(
#     cor_var1_target = target_cor_vec[Var1],
#     cor_var2_target = target_cor_vec[Var2],
#     drop = if_else(cor_var1_target >= cor_var2_target, Var2, Var1)
#   )
# 
# highly_correlated <- cor_pairs %>%
#   pull(drop) %>%
#   unique()
# 
# print(highly_correlated)

# =========================
# 7. Dropping useless Rows & Cols
# =========================

# ===
# 7.1 Utility functions
# ===

drop_cols_report <- function(data, cols_to_drop) {
  # 1. Store the initial count
  col_num_before <- ncol(data)
  
  # 2. Perform the drop
  # We use any_of() instead of all_of() to prevent errors 
  # if a column was already dropped previously.
  data_reduced <- data %>%
    select(!any_of(cols_to_drop))
  
  # 3. Calculate difference
  col_num_after <- ncol(data_reduced)
  dropped_count <- col_num_before - col_num_after
  
  # 4. Print the report
  message(paste0("Dropped cols: ", dropped_count, 
                 " - Current size: ", col_num_after))
  
  # 5. Return the cleaned dataframe
  return(data_reduced)
}

drop_rows_report <- function(data, rows_to_drop) {
  # 1. Store initial count
  row_num_before <- nrow(data)
  
  # 2. Perform the drop
  # Using -rows_to_drop handles the removal by index.
  # We check if rows_to_drop is empty first to avoid returning an empty table.
  if (length(rows_to_drop) > 0) {
    data_reduced <- data[-rows_to_drop, ]
  } else {
    data_reduced <- data
  }
  
  # 3. Calculate difference
  row_num_after <- nrow(data_reduced)
  dropped_count <- row_num_before - row_num_after
  
  # 4. Print the report
  message(paste0("Dropped rows: ", dropped_count, 
                 " - Current size: ", row_num_after))
  
  return(data_reduced)
}

# ===
# 7.2 Highly-missing columns
# ===

timetable_reduced <- drop_cols_report(timetable, high_missing)

# ===
# 7.3 Constant features
# ===

timetable_reduced <- drop_cols_report(timetable_reduced, constant_features)

# ===
# 7.4 Near-constant features
# ===

timetable_reduced <- drop_cols_report(timetable_reduced, nzv_cols)

# ===
# 7.5 Duplicate rows
# ===

timetable_reduced <- drop_rows_report(timetable_reduced, dup_row_indexes)

# ===
# 7.6 Duplicate cols
# ===

timetable_reduced <- drop_cols_report(timetable_reduced, dup_cols)

# # ===
# # 7.7 Highly-correlated cols
# # ===
# 
# timetable_reduced <- drop_cols_report(timetable_reduced, highly_correlated)

# glimpse(timetable_reduced)

# ===
# 7.8 Irrelevant cols
# ===
# these were used during extraction process and were broken into several predictors
# ===



highly_correlated_char = c('arrival_time', 'service_id', 'shape_id', 'stop_id',
                           'monday', 'tuesday', 'wednesday', 'thursday', 'friday',
                           'saturday', 'sunday', 'trip_id', 'stop_sequence')
timetable_reduced <- drop_cols_report(timetable_reduced, highly_correlated_char)

# ===
# 7.8 Irrelevant cols
# ===

# these should not be in the dataset after extraction update and remove from here!!
# irrelevant_features = c('day_type')

# timetable_reduced <- drop_cols_report(timetable_reduced, irrelevant_features)

# ===
# 7.9 Basic info after filtering
# ===
dim(timetable_reduced)
glimpse(timetable_reduced)



# =========================
# 8. Handling missing values
# =========================
low_missing

# ===
# 8.1 Time_since_last_stop
# ===
# These are first stops in a trip

timetable_imputed <- timetable_reduced %>%
  mutate(
    time_since_last_stop = replace_na(time_since_last_stop, 0)
  )

# ===
# 8.2 Wheelchair accessible
# ===

# here nans should be interpreted as 

timetable_imputed <- timetable_imputed %>%
  mutate(
    wheelchair_accessible = case_when(
      is.na(wheelchair_accessible) ~ "missing",
      wheelchair_accessible == 0 ~ "unknown",
      wheelchair_accessible == 1 ~ "accessible",
      wheelchair_accessible == 2 ~ "not_accessible",
      TRUE ~ "other"
    ),
    wheelchair_accessible = as.factor(wheelchair_accessible)
  )

paste0("Wheelchair_accessible after imputing missing vals")
table(timetable_imputed$wheelchair_accessible)

# ===
# 8.3 Basic info after handling missing vlaues
# ===
dim(timetable_imputed)
glimpse(timetable_imputed)



# =========================
# 9. Handling categorical values
# =========================

# Scan all character columns and count unique values
unique_counts <- timetable_imputed %>%
  summarise(across(where(is.character), ~n_distinct(.))) %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "unique_count")

print(unique_counts)

# =========================================================
# 1. One-hot encoding for low-cardinality predictors
#    Returns the original dataset with encoded columns added
#    and the original column removed.
# =========================================================
one_hot_encode <- function(data, col) {
  data[[col]] <- as.factor(data[[col]])
  
  mm <- model.matrix(
    as.formula(paste0("~ `", col, "` - 1")),
    data = data
  ) %>%
    as.data.frame()
  
  data %>%
    bind_cols(mm) %>%
    select(-all_of(col))
}

# =========================================================
# 2. Frequency encoding
#    Replaces each category by its relative frequency.
#    Good for medium/high-cardinality categoricals.
# =========================================================
frequency_encode <- function(data, col, new_col = NULL) {
  if (is.null(new_col)) {
    new_col <- paste0(col, "_freq")
  }
  
  freq_tbl <- data %>%
    count(.data[[col]], name = "n") %>%
    mutate(freq = n / sum(n)) %>%
    select(all_of(col), freq)
  
  data %>%
    left_join(freq_tbl, by = col) %>%
    rename(!!new_col := freq)
}

# =========================================================
# 3. Hex color -> RGB
#    Good for route_color if you want numeric representation
#    without category explosion.
# =========================================================
hex_to_rgb_df <- function(data, col = "route_color", prefix = "route") {
  hex_to_rgb <- function(hex) {
    hex <- gsub("#", "", as.character(hex))
    if (is.na(hex) || nchar(hex) != 6) return(c(NA, NA, NA))
    c(
      strtoi(substr(hex, 1, 2), 16L),
      strtoi(substr(hex, 3, 4), 16L),
      strtoi(substr(hex, 5, 6), 16L)
    )
  }
  
  rgb_vals <- t(sapply(data[[col]], hex_to_rgb)) %>%
    as.data.frame()
  
  colnames(rgb_vals) <- c(
    paste0(prefix, "_r"),
    paste0(prefix, "_g"),
    paste0(prefix, "_b")
  )
  
  bind_cols(data, rgb_vals)
}

table(timetable_encoded$day_typeweekday)



timetable_encoded <- timetable_imputed %>%
  one_hot_encode("platform_code") %>%
  one_hot_encode("day_type") %>%
  one_hot_encode("zone_id") %>%
  one_hot_encode("direction_id") %>%
  one_hot_encode("wheelchair_accessible") %>%
  one_hot_encode("pickup_type") %>%
  one_hot_encode("route_type") %>%
  hex_to_rgb_df("route_color", prefix = "route") %>%
  frequency_encode("route_short_name") %>%
  frequency_encode("route_id") %>%
  frequency_encode("trip_headsign") %>%
  frequency_encode("stop_name") %>%
  frequency_encode("stop_code") %>%
  select(
    -route_color,
    -route_short_name,
    -route_id,
    -trip_headsign,
    -stop_name,
    -stop_code
  )


# ===
# 9.1 Check duplicate cols again
# ===

dup_cols = find_duplicate_columns(timetable_encoded)
print(dup_cols)

timetable_encoded <- drop_cols_report(timetable_encoded, dup_cols)

glimpse(timetable_encoded)
table(timetable_encoded$platform_codeI)





# =========================
# 10. Save dataset
# =========================
saveRDS(timetable_encoded, "./datasets/timetable_encoded.rds")


# ===
# 9.1 Renaming days
# ===
# these represent if trip takes place on that day
# monday actually represnt weekday

# timetable_imputed <- timetable_imputed %>%
#   rename(weekday = monday)
# 
# table(timetable_imputed$day_type)

# ===
# 9.2 char cols with high cardinality
# ===
# 
# timetable_encoded <- timetable_imputed %>%
#   separate(
#     trip_id,
#     into = c("trip_part_1", "trip_part_2", "trip_part_3", "trip_part_4"),
#     sep = "_",
#     convert = TRUE
#   )
# 
# # Dropping high cardinlity trip parts
# 
# cols_to_drop = c('trip_part_3', 'trip_part_4')
# 
# timetable_encoded <- timetable_encoded %>%
#   select(-all_of(cols_to_drop))
# 
# glimpse(timetable_encoded)

# ===
# 9.3 Cols with low cardinality
# ===

# choose low-cardinality categorical columns
# low_cardinality_cols <- c(
#   # "trip_headsign",
#   "wheelchair_accessible",
#   "platform_code",
#   # "route_short_name",
#   # "stop_name",
#   # "route_color",
#   # 'trip_part_1',
#   # 'trip_part_2',
#   'day_type'
# )
# 
# 
# # convert to factors
# timetable_encoded <- timetable_encoded %>%
#   mutate(across(all_of(low_cardinality_cols), as.factor))
# 
# # one-hot encode only these columns
# dummy_matrix <- model.matrix(
#   ~ trip_headsign + wheelchair_accessible + platform_code + route_short_name
#   + stop_name + route_color + trip_part_1 + trip_part_2 + day_type - 1,
#   data = timetable_encoded
# ) %>%
#   as.data.frame()
# 
# # bind encoded columns and drop originals
# timetable_encoded <- timetable_encoded %>%
#   select(-all_of(low_cardinality_cols)) %>%
#   bind_cols(dummy_matrix)



# =========================
# 10. Correlations
# =========================

# ===
# 10.1 Load dataset
# ===



# ===
# 10.1 Correlation filtering (mainly for regression models)
# ===

timetable_encoded <- readRDS("./datasets/timetable_encoded.rds")

glimpse(timetable_encoded)

select_correlated_features <- function(
    data,
    target_col,
    min_abs_cor = 0.1,
    top_n = NULL,
    always_keep = NULL
) {
  # keep only numeric columns for correlation
  numeric_data <- data %>%
    dplyr::select(where(is.numeric))
  
  # check target exists
  if (!(target_col %in% names(numeric_data))) {
    stop(paste("Target column", target_col, "must exist and be numeric."))
  }
  
  # compute correlations with target
  cor_vec <- cor(numeric_data, use = "complete.obs")[, target_col]
  
  cor_df <- data.frame(
    feature = names(cor_vec),
    correlation = as.numeric(cor_vec)
  ) %>%
    dplyr::filter(feature != target_col) %>%
    dplyr::mutate(abs_correlation = abs(correlation)) %>%
    dplyr::arrange(desc(abs_correlation))
  
  # threshold filter
  cor_df <- cor_df %>%
    dplyr::filter(abs_correlation >= min_abs_cor)
  
  # optional top_n restriction
  if (!is.null(top_n)) {
    cor_df <- cor_df %>%
      dplyr::slice_head(n = top_n)
  }
  
  # final feature list
  selected_features <- unique(c(
    target_col,
    cor_df$feature,
    always_keep
  ))
  
  # keep only existing columns (safe)
  selected_features <- intersect(selected_features, names(data))
  
  return(data %>%
           dplyr::select(all_of(selected_features)))
}
  
  # return(cor_df)
# }
# 
subset_data <- select_correlated_features(
  data = timetable_encoded,
  target_col = "segment_time_s",
  min_abs_cor = 0.05,
  always_keep = c("seconds_since_midnight", "route_short_name_freq")
)

glimpse(subset_data)


# subset_data <- timetable_encoded %>%
#   select(all_of(append('segment_time_s', selected_cor_df %>% pull(feature))))

# ===
# 10.1 Remove multicollinearity
# ===

# collinear_features <- c('route_short_name139', 'trip_part_1139009',
# 'route_colorF56200', 'trip_headsignCintorín Slávičie')
collinear_features <- c()

subset_data_filtered <- subset_data %>%
  select(-all_of(collinear_features))
# 
# glimpse(subset_data_filtered)
# ===
# 10.2 Corr matrix
# ===
# 
# cor_matrix <- cor(subset_data_filtered, use = "complete.obs")
# 
# cor_long <- as.data.frame(as.table(cor_matrix))
# colnames(cor_long) <- c("Var1", "Var2", "Correlation")
# 
# # ===
# # 10.3 Plotting heatmap
# # ===
# 
# ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
#   geom_tile() +
#   
#   # this adds correlation values inside the heatmap but only those above
#   # certain value to improve readability
#   geom_text(
#     aes(label = ifelse(abs(Correlation) > 0.3, round(Correlation, 2), "")),
#     size = 3
#   ) +
#   
#   scale_fill_gradient2(
#     low = "blue",
#     mid = "white",
#     high = "red",
#     midpoint = 0
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title = element_blank()
#   ) +
#   ggtitle("Correlation Heatmap (Filtered Labels)")
plot_correlation_heatmap <- function(df, threshold = 0.3, title = "Correlation Heatmap") {
  
  # 1. Calculate correlation matrix (only for numeric columns)
  # It's safer to filter for numeric variables automatically
  cor_matrix <- df %>%
    select(where(is.numeric)) %>%
    cor(use = "complete.obs")
  
  # 2. Convert to long format
  cor_long <- as.data.frame(as.table(cor_matrix))
  colnames(cor_long) <- c("Var1", "Var2", "Correlation")
  
  # 3. Create the plot
  ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    # Dynamic label filtering based on the 'threshold' argument
    geom_text(
      aes(label = ifelse(abs(Correlation) > threshold, round(Correlation, 2), "")),
      size = 3,
      color = "black"
    ) +
    scale_fill_gradient2(
      low = "#377eb8",  # Nice blue
      mid = "white",
      high = "#e41a1c", # Nice red
      midpoint = 0,
      limit = c(-1, 1)   # Ensures the scale is always centered
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    ggtitle(title)
}

# plot_correlation_heatmap(subset_data_filtered, threshold = 0.1)

# =========================
# 11. Outliers
# =========================

# ===
# 11.1 Visualization
# ===

# ==
# 11.1.1 Boxplots
# ==

# Convert to long format
# subset_long <- subset_data_filtered %>%
#   pivot_longer(
#     cols = -segment_time_s,
#     names_to = "feature",
#     values_to = "value"
#   )

# Boxplots
# ggplot(subset_long, aes(x = feature, y = value)) +
#   geom_boxplot(outlier.color = "red", outlier.alpha = 0.5) +
#   coord_flip() +
#   theme_minimal() +
#   ggtitle("Boxplots of Features (Outlier Detection)")
# 
# Reusable plotting function
# Reusable function for boxplots
plot_box_dist <- function(df, var_name) {
  ggplot(df, aes(x = !!sym(var_name))) +
    # Adding a dummy y-axis value or a factor(0) helps boxplot orientation
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.alpha = 0.5) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Feature:", var_name),
      x = NULL # Removing x label as the title covers it
    )
}
# # 1. Identify columns (all except the time/ID column)
# is_binary <- map_lgl(subset_data_filtered, ~length(unique(na.omit(.x))) <= 2)
# binary_cols <- names(is_binary[is_binary])
# 
# # 2. Identify the predictors you actually want to plot
# # We exclude 'segment_time_s' AND all identified binary columns
# predictors_to_plot <- setdiff(names(subset_data_filtered), c("segment_time_s", binary_cols))
# 
# # 3. Print a little heads-up so you know what was dropped
# message("Dropping binary features: ", paste(binary_cols, collapse = ", "))
# # 2. Use map to create a list of individual plots
# # .x passes the column name string to the var_name argument
# plot_list <- map(predictors_to_plot, ~plot_box_dist(subset_data_filtered, .x))
# 
# # 3. Combine with patchwork
# # wrap_plots handles the list directly
# combined_plot <- wrap_plots(plot_list, ncol = 3) + 
#   plot_annotation(
#     title = "Outlier Detection Across Predictors",
#     subtitle = "Boxplots identifying potential outliers in red",
#     theme = theme(plot.title = element_text(size = 18, face = "bold"))
#   )
# 
# # 4. Display
# print(combined_plot)
plot_predictor_boxplots <- function(
    df,
    target_col = "segment_time_s",
    ncol = 3,
    show_message = TRUE,
    title = "Outlier Detection Across Predictors",
    subtitle = "Boxplots identifying potential outliers in red"
) {
  # Identify binary columns
  is_binary <- purrr::map_lgl(df, ~ length(unique(stats::na.omit(.x))) <= 2)
  binary_cols <- names(is_binary[is_binary])
  
  # Select predictors to plot
  predictors_to_plot <- setdiff(names(df), c(target_col, binary_cols))
  
  # Optional message
  if (show_message) {
    message("Dropping binary features: ", paste(binary_cols, collapse = ", "))
  }
  
  # Create individual plots
  plot_list <- purrr::map(predictors_to_plot, ~ plot_box_dist(df, .x))
  
  # Combine plots
  combined_plot <- patchwork::wrap_plots(plot_list, ncol = ncol) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold")
      )
    )
  
  return(combined_plot)
}

combined_plot <- plot_predictor_boxplots(subset_data_filtered)
print(combined_plot)

# summary(subset_data_transformed %>% select(all_of(predictors_to_plot)))

# # ==
# # 11.1.2 Histograms
# # ==
# 
# ggplot(subset_long, aes(x = value)) +
#   geom_histogram(bins = 50, fill = "grey", color = "black") +
#   facet_wrap(~ feature, scales = "free") +
#   theme_minimal() +
#   ggtitle("Feature Distributions")
# 
# 
# ggplot(subset_long, aes(x = value, y = segment_time_s)) +
#   geom_point(alpha = 0.2) +
#   facet_wrap(~ feature, scales = "free_x") +
#   theme_minimal() +
#   ggtitle("Feature vs Target (Outlier Detection)")

# ===
# 11.2 Transformations
# ===

# ==
# 11.2.1 Log-scale
# ==

# transforming segment_distnace_m as it is skewed, others are fine
# subset_data_filtered <- subset_data_filtered %>%
#   select(-all_of(c('segment_distance_log')))

problematic_features <- c('segment_distance_m', 'time_since_last_stop')

# summary(subset_data_filtered$segment_distance_m)
# summary(subset_data_filtered$time_since_last_stop)
# summary(subset_data_filtered$distance_time_interaction)

subset_data_transformed <- subset_data_filtered %>%
  mutate(across(all_of(problematic_features), log1p))

combined_plot <- plot_predictor_boxplots(subset_data_transformed)
print(combined_plot)

glimpse(subset_data_transformed)

# =========================
# 10. Data Engineering
# =========================

# seconds since midnight problem -> 23 -> far from 0, these are better

timetable_engineered <- subset_data_filtered %>%
  mutate(
    sin_time = sin(2 * pi * seconds_since_midnight / 86400),
    cos_time = cos(2 * pi * seconds_since_midnight / 86400),
    distance_sin_interaction = segment_distance_m * sin_time,
    distance_cos_interaction = segment_distance_m * cos_time,
  )

# timetable_engineered <- timetable_engineered %>%
#   group_by(route_short_name_freq) %>%
#   mutate(route_avg_time = mean(segment_time_s))


glimpse(timetable_engineered)

# ===
# Heatmap again
# ===

timetable_engineered <- select_correlated_features(
  data = timetable_engineered,
  target_col = "segment_time_s",
  min_abs_cor = 0.05
)


# timetable_engineered <- select_correlated_features(
#   data = timetable_engineered,
#   target_col = "segment_time_s",
#   min_abs_cor = 0.05
# )
# 
# subset_data <- timetable_encoded %>%
#   select(all_of(append('segment_time_s', selected_cor_df %>% pull(feature))))

collinear_features <- c('zone_id101', 'cos_time', 'route_type0', 'stop_code_freq')

timetable_engineered <- timetable_engineered %>%
  select(-all_of(collinear_features))

plot_correlation_heatmap(timetable_engineered, threshold = 0.1)



# ===
# Boxplots again
# ===

combined_plot <- plot_predictor_boxplots(timetable_engineered)
print(combined_plot)

summary(timetable_engineered$segment_distance_m)
summary(timetable_engineered$distance_cos_interaction)


# ==
# 11.2.2 Feature engineering
# ==

# subset_data_transformed <- subset_data_transformed %>%
#   select(-all_of(c('segment_distance_log')))

# subset_data_transformed <- subset_data_transformed %>%
#   mutate(distance_time_interaction = segment_distance_m * arrival_time_hour)
# 
# subset_data_transformed <- subset_data_transformed %>%
#   mutate(speed_est = segment_distance_m / (time_since_last_stop + 1))


# plot_correlation_heatmap(subset_data_transformed)


# =========================
# 12. Model Training
# =========================



# ===
# 12.1 Train / test split
# ===

glimpse(timetable_engineered)

set.seed(42)

# 80/20 split
train_idx <- sample(seq_len(nrow(timetable_engineered)), size = 0.8 * nrow(timetable_engineered))

train_data <- timetable_engineered[train_idx, ]
test_data  <- timetable_engineered[-train_idx, ]

# ===
# 12.2 Linear regression
# ===

# unimportant_features <- c('time_since_last_stop', 'platform_codeH', 'arrival_seconds')
unimportant_features <- c('route_b', 'arrival_seconds', 'route_short_name_freq')


train_data <- train_data %>%
  select(-all_of(unimportant_features))

test_data <- test_data %>%
  select(-all_of(unimportant_features))

lm_model <- lm(segment_time_s ~ ., data = train_data)
summary(lm_model)

# test data

predictions <- predict(lm_model, newdata = test_data)

rmse <- sqrt(mean((test_data$segment_time_s - predictions)^2))
rmse

mae <- mean(abs(test_data$segment_time_s - predictions))
mae

ss_total <- sum((test_data$segment_time_s - mean(test_data$segment_time_s))^2)
ss_res   <- sum((test_data$segment_time_s - predictions)^2)

r2_test <- 1 - (ss_res / ss_total)
r2_test

library(ggplot2)

ggplot(data.frame(actual = test_data$segment_time_s, pred = predictions),
       aes(x = actual, y = pred)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  ggtitle("Predicted vs Actual")


ggplot(data.frame(pred = predictions, residuals = test_data$segment_time_s - predictions),
       aes(x = pred, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  ggtitle("Residuals vs Predictions")


