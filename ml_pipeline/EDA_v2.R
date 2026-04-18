# libraries
library("tidyverse")
library("readr")
library("ggplot2")

# =========================
# Load data
# =========================
timetable <- read_csv("../data/interim/dataset.csv")
table(timetable$day_type)

# =========================
# Restrict dataset for testing purposes
# =========================

timetable <- timetable %>% head(n=10000)

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

# More efficient check for duplicate columns
# R looks at the "top level" of the data frame. duplicated() now looks at each 
# vector (column) as a single item in a list and compares them.
dup_col_indexes <- which(duplicated(as.list(timetable)))
dup_cols <- names(timetable)[dup_col_indexes]

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
target_cor <- timetable %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

target_cor_vec <- abs(target_cor[, "segment_time_s"])

cor_pairs <- cor_pairs %>%
  mutate(
    cor_var1_target = target_cor_vec[Var1],
    cor_var2_target = target_cor_vec[Var2],
    drop = if_else(cor_var1_target >= cor_var2_target, Var2, Var1)
  )

highly_correlated <- cor_pairs %>%
  pull(drop) %>%
  unique()

print(highly_correlated)

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

# ===
# 7.7 Highly-correlated cols
# ===

timetable_reduced <- drop_cols_report(timetable_reduced, highly_correlated)

# ===
# 7.8 Highly correlated char cols
# ===
# these were broken into multiple numerical cols, they would be hihghly correlated
# ===

highly_correlated_char = c('arrival_time', 'service_id', 'shape_id', 'stop_id',
                           'monday', 'tuesday', 'wednesday', 'thursday', 'friday',
                           'saturday', 'sunday')
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

# ===
# 9.1 Renaming days
# ===
# these represent if trip takes place on that day
# monday actually represnt weekday

timetable_imputed <- timetable_imputed %>%
  rename(weekday = monday)

table(timetable_imputed$day_type)

# ===
# 9.2 char cols with high cardinality
# ===

timetable_encoded <- timetable_imputed %>%
  separate(
    trip_id,
    into = c("trip_part_1", "trip_part_2", "trip_part_3", "trip_part_4"),
    sep = "_",
    convert = TRUE
  )

# Dropping high cardinlity trip parts

cols_to_drop = c('trip_part_3', 'trip_part_4')

timetable_encoded <- timetable_encoded %>%
  select(-all_of(cols_to_drop))

# ===
# 9.3 Cols with low cardinality
# ===

# choose low-cardinality categorical columns
low_cardinality_cols <- c(
  "trip_headsign",
  "wheelchair_accessible",
  "platform_code",
  "route_short_name",
  "stop_name",
  "route_color",
  'trip_part_1',
  'trip_part_2',
  'day_type'
)


# convert to factors
timetable_encoded <- timetable_encoded %>%
  mutate(across(all_of(low_cardinality_cols), as.factor))

# one-hot encode only these columns
dummy_matrix <- model.matrix(
  ~ trip_headsign + wheelchair_accessible + platform_code + route_short_name
  + stop_name + route_color + trip_part_1 + trip_part_2 + day_type - 1,
  data = timetable_encoded
) %>%
  as.data.frame()

# bind encoded columns and drop originals
timetable_encoded <- timetable_encoded %>%
  select(-all_of(low_cardinality_cols)) %>%
  bind_cols(dummy_matrix)

glimpse(timetable_encoded)
colnames(timetable_encoded)


# =========================
# 10. Correlations
# =========================

# ===
# 10.1 Selecting predictors
# ===

select_correlated_features <- function(data, target_col, min_abs_cor = 0.1, top_n = NULL) {
  # keep only numeric columns
  numeric_data <- data %>%
    select(where(is.numeric))
  
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
    filter(feature != target_col) %>%
    mutate(abs_correlation = abs(correlation)) %>%
    arrange(desc(abs_correlation))
  
  # threshold filter
  cor_df <- cor_df %>%
    filter(abs_correlation >= min_abs_cor)
  
  # optional top_n restriction
  if (!is.null(top_n)) {
    cor_df <- cor_df %>%
      slice_head(n = top_n)
  }
  
  return(cor_df)
}

# selected_features <- c(
#   # Target
#   "segment_time_s",
#   
#   # Core signal
#   "segment_distance_m",
#   "time_since_last_stop",
#   
#   # Temporal
#   "arrival_time_hour",
#   "arrival_time_minute",
#   
#   # Spatial
#   "stop_lat",
#   "stop_lon",
#   
#   # Trip context
#   "direction_id",
#   
#   # Encoded categorical (reduced set)
#   "trip_headsignHlavná stanica",
#   "trip_headsignNám. Ľ. Štúra",
#   
#   # Platform
#   "platform_codeB",
#   "platform_codeD",
#   
#   # Day type
#   "day_typeweekday",
#   "day_typesunday",
#   
#   # Route
#   "route_short_name139"
# )
selected_cor_df <- select_correlated_features(
  data = timetable_encoded,
  target_col = "segment_time_s",
  min_abs_cor = 0.2
)

print(selected_cor_df)

subset_data <- timetable_encoded %>%
  select(all_of(append('segment_time_s', selected_cor_df %>% pull(feature))))

# these are actually highly intercorrelated
drop_cols <- c('route_short_name139', 'trip_part_1139009', 'route_colorF56200')

subset_data <- subset_data %>%
  select(-all_of(drop_cols))

# ===
# 10.2 Corr matrix
# ===

cor_matrix <- cor(subset_data, use = "complete.obs")

cor_long <- as.data.frame(as.table(cor_matrix))
colnames(cor_long) <- c("Var1", "Var2", "Correlation")

# ===
# 10.3 Plotting heatmap
# ===

ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  ) +
  ggtitle("Correlation Heatmap (Selected Features + Target)")

