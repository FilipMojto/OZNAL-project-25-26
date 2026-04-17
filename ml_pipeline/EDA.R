# libraries
library("tidyverse")
library("readr")
library("ggplot2")

# =========================
# Load data
# =========================
timetable <- read_csv("../data/interim/dataset.csv")

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

print(missing_summary, n = 50)

# Filter columns with high missing values (>50%)
low_missing <- missing_summary %>%
  filter(missing_pct < 50)

print(low_missing, n = 50)

# restrict data to only at least half-full columns 
# Extract the names of the "good" columns
valid_columns <- low_missing %>% pull(column)

# Subset the original dataframe
timetable <- timetable %>%
  select(all_of(valid_columns))



# =========================
# 2. Constant / low variance columns
# =========================
variance_summary <- timetable %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "n_unique") %>%
  arrange(n_unique)

print(variance_summary, n = 50)

# Constant columns
non_constant_cols <- variance_summary %>%
  filter(n_unique > 1)


valid_columns <- non_constant_cols %>% pull(column)
timetable <- timetable %>%
  select(all_of(valid_columns))

View(timetable)

print(constant_cols)

# =========================
# 3. Near-zero variance (important)
# =========================
# nzv_summary <- timetable %>%
#   summarise(across(everything(), ~ {
#     freq <- sort(table(.), decreasing = TRUE)
#     if (length(freq) <= 1) return(NA)
#     freq[1] / freq[2]
#   })) %>%
#   pivot_longer(cols = everything(), names_to = "column", values_to = "freq_ratio") %>%
#   arrange(desc(freq_ratio))
# 
# print(nzv_summary, n=50)
# 
# # Threshold example: freq_ratio > 20
# nzv_cols <- nzv_summary %>%
#   filter(freq_ratio > 20)
# 
# print(nzv_cols)

# =========================
# 4. Duplicate rows
# =========================
dup_rows <- timetable %>%
  duplicated() %>%
  sum()

print(paste("Duplicate rows:", dup_rows))

# =========================
# 5. Duplicate columns
# =========================
dup_cols <- which(duplicated(as.list(timetable)))

print(names(timetable)[dup_cols])

# =========================
# 6. High cardinality categorical variables
# =========================
cat_summary <- timetable %>%
  select(where(is.character)) %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "n_unique") %>%
  arrange(desc(n_unique))

print(cat_summary)

# Example: columns with too many categories
high_cardinality <- cat_summary %>%
  filter(n_unique > 100)

print(high_cardinality)

# =========================
# 7. Correlation (numeric only)
# =========================
numeric_data <- timetable %>%
  select(where(is.numeric)) %>%
  select(-segment_time_s)  # exclude target

cor_matrix <- cor(numeric_data, use = "complete.obs")

# Find highly correlated pairs
cor_pairs <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%
  filter(abs(Freq) > 0.9) %>%
  arrange(desc(abs(Freq)))

print(cor_pairs)

invalid_columns <- c('drop_off_type', 'departure_seconds', 'zone_id',
                   'monday', 'tuesday', 'wednesday', 'thursday', 'friday',
                   'saturday', 'sunday', 'stop_id','arrival_time_hour', 'trip_id',
                   'arrival_time', 'departure_time', 'stop_lat', 'stop_lon', 'stop_sequence',
                   'stop_name', 'shape_id', 'route_id', 'service_id')
# This keeps everything EXCEPT the columns in your list
timetable <- timetable %>%
  select(!all_of(invalid_columns))


View(timetable)
# =========================
# 8. Basic distribution check (target)
# =========================
ggplot(timetable, aes(x = segment_time_s)) +
  geom_histogram(bins = 50) +
  ggtitle("Distribution of segment_time_s")

# =========================
# 9. Speed distribution (sanity)
# =========================
if ("speed_kmh" %in% colnames(timetable)) {
  ggplot(timetable, aes(x = speed_kmh)) +
    geom_histogram(bins = 50) +
    xlim(0, 100) +
    ggtitle("Speed distribution (km/h)")
}

# =========================
# 10. Summary recommendations
# =========================
cat("\n--- SUMMARY ---\n")

cat("\nColumns with >50% missing:\n")
print(high_missing$column)

cat("\nConstant columns:\n")
print(constant_cols$column)

cat("\nNear-zero variance columns:\n")
print(nzv_cols$column)

cat("\nHigh-cardinality categorical columns:\n")
print(high_cardinality$column)


# 11. Encoding characters
timetable <- timetable %>%
  mutate(across(where(is.character), as.factor))

timetable_encoded <- model.matrix(~ . - 1, data = timetable) %>%
  as.data.frame()

View(timetable_encoded)




