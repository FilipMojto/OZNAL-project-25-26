import os
from pathlib import Path

import numpy as np
import pandas as pd
from dotenv import load_dotenv

from config import INTERIM_DIR

# =========================
# Configuration
# =========================
load_dotenv()

INPUT_FILE = Path(INTERIM_DIR) / "dataset.csv"
OUTPUT_DIR = Path(INTERIM_DIR) / "validation_reports"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

TARGET_COL = "segment_time_s"
DIST_COL = "segment_distance_m"

TIME_COLS = [
    "arrival_seconds",
    "departure_seconds",
    "arrival_time_hour",
    "arrival_time_minute",
    "arrival_time_second",
    "seconds_since_midnight",
    "time_since_last_stop",
]

FLAG_COLS = ["is_weekday", "is_saturday", "is_sunday"]

KEY_COLS = ["trip_id", "stop_sequence", "stop_id"]

# Reasonable thresholds for sanity checks
MAX_SEGMENT_TIME_S = 1800      # 30 min between consecutive stops is suspicious
MAX_SEGMENT_DISTANCE_M = 5000  # > 5 km between consecutive city stops is suspicious
MAX_SPEED_KMH = 100            # unrealistic for city public transport
MIN_SPEED_KMH = 1              # suspiciously slow


# =========================
# Helpers
# =========================
def print_section(title: str):
    print("\n" + "=" * 80)
    print(title)
    print("=" * 80)


def safe_describe(df: pd.DataFrame, cols: list[str]) -> pd.DataFrame:
    existing = [c for c in cols if c in df.columns]
    if not existing:
        return pd.DataFrame()
    return df[existing].describe(include="all").T


def save_csv(df: pd.DataFrame, filename: str):
    out_path = OUTPUT_DIR / filename
    df.to_csv(out_path, index=False)
    print(f"Saved: {out_path}")


def parse_hex_color(value):
    if pd.isna(value):
        return np.nan, np.nan, np.nan
    value = str(value).strip().replace("#", "")
    if len(value) != 6:
        return np.nan, np.nan, np.nan
    try:
        r = int(value[0:2], 16)
        g = int(value[2:4], 16)
        b = int(value[4:6], 16)
        return r, g, b
    except ValueError:
        return np.nan, np.nan, np.nan


# =========================
# Load data
# =========================
print_section("LOADING DATA")
print(f"Input file: {INPUT_FILE}")

dataset = pd.read_csv(INPUT_FILE)
print("Loaded successfully.")


# =========================
# 1. Basic structure
# =========================
print_section("1. BASIC STRUCTURE")

print(f"Shape: {dataset.shape}")
print("\nColumns:")
for col in dataset.columns:
    print(f" - {col}")

print("\nDtypes:")
print(dataset.dtypes)

schema_df = pd.DataFrame(
    {
        "column": dataset.columns,
        "dtype": dataset.dtypes.astype(str).values,
        "non_null_count": dataset.notna().sum().values,
        "null_count": dataset.isna().sum().values,
        "null_ratio": (dataset.isna().mean().values * 100).round(2),
        "n_unique": dataset.nunique(dropna=True).values,
    }
).sort_values(["null_ratio", "n_unique"], ascending=[False, False])

save_csv(schema_df, "schema_summary.csv")


# =========================
# 2. Missing values
# =========================
print_section("2. MISSING VALUES")

missing = dataset.isna().sum().sort_values(ascending=False)
missing = missing[missing > 0]

if missing.empty:
    print("No missing values found.")
else:
    missing_df = pd.DataFrame(
        {
            "column": missing.index,
            "missing_count": missing.values,
            "missing_ratio_pct": (missing.values / len(dataset) * 100).round(2),
        }
    )
    print(missing_df.to_string(index=False))
    save_csv(missing_df, "missing_values.csv")


# =========================
# 3. Duplicate rows
# =========================
print_section("3. DUPLICATES")

dup_count = dataset.duplicated().sum()
print(f"Exact duplicate rows: {dup_count}")

if dup_count > 0:
    dup_rows = dataset[dataset.duplicated(keep=False)].copy()
    save_csv(dup_rows, "duplicate_rows.csv")


# =========================
# 4. Numeric summaries
# =========================
print_section("4. NUMERIC COLUMN SUMMARIES")

numeric_cols = dataset.select_dtypes(include=[np.number]).columns.tolist()
numeric_summary = safe_describe(dataset, numeric_cols)
if not numeric_summary.empty:
    print(numeric_summary.to_string())
    numeric_summary.reset_index().rename(columns={"index": "column"}).to_csv(
        OUTPUT_DIR / "numeric_summary.csv", index=False
    )


# =========================
# 5. Categorical summaries
# =========================
print_section("5. CATEGORICAL COLUMN SUMMARIES")

categorical_cols = dataset.select_dtypes(include=["object"]).columns.tolist()
cat_rows = []

for col in categorical_cols:
    non_null = dataset[col].dropna()
    top_values = non_null.value_counts().head(10)
    cat_rows.append(
        {
            "column": col,
            "non_null_count": non_null.shape[0],
            "n_unique": non_null.nunique(),
            "top_value": top_values.index[0] if not top_values.empty else np.nan,
            "top_value_count": int(top_values.iloc[0]) if not top_values.empty else np.nan,
        }
    )

cat_summary = pd.DataFrame(cat_rows).sort_values("n_unique", ascending=False)
if not cat_summary.empty:
    print(cat_summary.to_string(index=False))
    save_csv(cat_summary, "categorical_summary.csv")


# =========================
# 6. Key-column sanity
# =========================
print_section("6. KEY COLUMN SANITY")

for col in KEY_COLS:
    if col in dataset.columns:
        print(f"{col}: non-null={dataset[col].notna().sum()}, unique={dataset[col].nunique(dropna=True)}")
    else:
        print(f"{col}: NOT FOUND")


# =========================
# 7. Target validation
# =========================
print_section("7. TARGET VALIDATION")

suspicious_parts = []

if TARGET_COL in dataset.columns:
    print(dataset[TARGET_COL].describe())

    invalid_target = dataset[dataset[TARGET_COL].isna()].copy()
    non_positive_target = dataset[dataset[TARGET_COL] <= 0].copy()
    huge_target = dataset[dataset[TARGET_COL] > MAX_SEGMENT_TIME_S].copy()

    print(f"Missing {TARGET_COL}: {len(invalid_target)}")
    print(f"Non-positive {TARGET_COL}: {len(non_positive_target)}")
    print(f"{TARGET_COL} > {MAX_SEGMENT_TIME_S}: {len(huge_target)}")

    if len(invalid_target):
        invalid_target["issue"] = f"missing_{TARGET_COL}"
        suspicious_parts.append(invalid_target)
    if len(non_positive_target):
        non_positive_target["issue"] = f"non_positive_{TARGET_COL}"
        suspicious_parts.append(non_positive_target)
    if len(huge_target):
        huge_target["issue"] = f"too_large_{TARGET_COL}"
        suspicious_parts.append(huge_target)
else:
    print(f"{TARGET_COL} not found.")


# =========================
# 8. Distance validation
# =========================
print_section("8. DISTANCE VALIDATION")

if DIST_COL in dataset.columns:
    print(dataset[DIST_COL].describe())

    invalid_dist = dataset[dataset[DIST_COL].isna()].copy()
    non_positive_dist = dataset[dataset[DIST_COL] <= 0].copy()
    huge_dist = dataset[dataset[DIST_COL] > MAX_SEGMENT_DISTANCE_M].copy()

    print(f"Missing {DIST_COL}: {len(invalid_dist)}")
    print(f"Non-positive {DIST_COL}: {len(non_positive_dist)}")
    print(f"{DIST_COL} > {MAX_SEGMENT_DISTANCE_M}: {len(huge_dist)}")

    if len(invalid_dist):
        invalid_dist["issue"] = f"missing_{DIST_COL}"
        suspicious_parts.append(invalid_dist)
    if len(non_positive_dist):
        non_positive_dist["issue"] = f"non_positive_{DIST_COL}"
        suspicious_parts.append(non_positive_dist)
    if len(huge_dist):
        huge_dist["issue"] = f"too_large_{DIST_COL}"
        suspicious_parts.append(huge_dist)
else:
    print(f"{DIST_COL} not found.")


# =========================
# 9. Speed validation
# =========================
print_section("9. SPEED VALIDATION")

if TARGET_COL in dataset.columns and DIST_COL in dataset.columns:
    dataset = dataset[dataset["segment_time_s"] > 0]
    dataset["speed_mps"] = dataset[DIST_COL] / dataset[TARGET_COL]
    dataset["speed_kmh"] = dataset["speed_mps"] * 3.6

    print(dataset["speed_kmh"].describe())

    invalid_speed = dataset[
        dataset["speed_kmh"].isna() | np.isinf(dataset["speed_kmh"])
    ].copy()
    
    too_fast = dataset[dataset["speed_kmh"] > MAX_SPEED_KMH].copy()
    too_slow = dataset[(dataset["speed_kmh"] > 0) & (dataset["speed_kmh"] < MIN_SPEED_KMH)].copy()

    print(f"Invalid speed rows: {len(invalid_speed)}")
    print(f"Too fast (> {MAX_SPEED_KMH} km/h): {len(too_fast)}")
    print(f"Too slow (< {MIN_SPEED_KMH} km/h): {len(too_slow)}")

    if len(invalid_speed):
        invalid_speed["issue"] = "invalid_speed"
        suspicious_parts.append(invalid_speed)
    if len(too_fast):
        too_fast["issue"] = "too_fast"
        suspicious_parts.append(too_fast)
    if len(too_slow):
        too_slow["issue"] = "too_slow"
        suspicious_parts.append(too_slow)

    too_fast["speed_excess_kmh"] = too_fast["speed_kmh"] - MAX_SPEED_KMH
    too_slow["speed_deficit_kmh"] = MIN_SPEED_KMH - too_slow["speed_kmh"]

    if len(too_fast):
        print("\nToo fast exceedance stats:")
        print(too_fast["speed_excess_kmh"].describe())

    if len(too_slow):
        print("\nToo slow deficit stats:")
        print(too_slow["speed_deficit_kmh"].describe())
else:
    print("Speed cannot be computed because target or distance column is missing.")


# =========================
# 10. Time consistency
# =========================
print_section("10. TIME CONSISTENCY")

if "arrival_seconds" in dataset.columns and "departure_seconds" in dataset.columns:
    bad_time_order = dataset[dataset["arrival_seconds"] > dataset["departure_seconds"]].copy()
    print(f"Rows where arrival_seconds > departure_seconds: {len(bad_time_order)}")

    if len(bad_time_order):
        bad_time_order["issue"] = "arrival_after_departure_same_stop"
        suspicious_parts.append(bad_time_order)
else:
    print("arrival_seconds and/or departure_seconds not found.")

for col in TIME_COLS:
    if col in dataset.columns:
        neg_rows = dataset[dataset[col].notna() & (dataset[col] < 0)].copy()
        print(f"{col}: negative values = {len(neg_rows)}")
        if len(neg_rows):
            neg_rows["issue"] = f"negative_{col}"
            suspicious_parts.append(neg_rows)


# =========================
# 11. Weekday flag consistency
# =========================
print_section("11. WEEKDAY FLAG CONSISTENCY")

if all(col in dataset.columns for col in FLAG_COLS):
    dataset["day_flag_sum"] = dataset[FLAG_COLS].sum(axis=1)
    bad_flags = dataset[dataset["day_flag_sum"] != 1].copy()

    print("Flag sums:")
    print(dataset["day_flag_sum"].value_counts(dropna=False).sort_index())

    print(f"Rows where weekday flags do not sum to 1: {len(bad_flags)}")

    if len(bad_flags):
        bad_flags["issue"] = "invalid_day_flags"
        suspicious_parts.append(bad_flags)
else:
    print("Some weekday flag columns are missing.")


# =========================
# 12. Stop sequence consistency
# =========================
print_section("12. STOP SEQUENCE CONSISTENCY")

if "trip_id" in dataset.columns and "stop_sequence" in dataset.columns:
    seq_df = dataset.sort_values(["trip_id", "stop_sequence"]).copy()
    seq_df["stop_seq_diff"] = seq_df.groupby("trip_id")["stop_sequence"].diff()

    seq_diff_counts = seq_df["stop_seq_diff"].value_counts(dropna=False).sort_index()
    print("Difference distribution in stop_sequence within trip:")
    print(seq_diff_counts.to_string())

    bad_sequence = seq_df[
        seq_df["stop_seq_diff"].notna() & (seq_df["stop_seq_diff"] <= 0)
    ].copy()
    print(f"Rows with non-increasing stop_sequence within trip: {len(bad_sequence)}")

    if len(bad_sequence):
        bad_sequence["issue"] = "non_increasing_stop_sequence"
        suspicious_parts.append(bad_sequence)

    # Check duplicated stop_sequence inside same trip
    duplicated_trip_stopseq = seq_df.duplicated(subset=["trip_id", "stop_sequence"], keep=False)
    dup_seq_rows = seq_df[duplicated_trip_stopseq].copy()
    print(f"Rows with duplicated (trip_id, stop_sequence): {len(dup_seq_rows)}")

    if len(dup_seq_rows):
        dup_seq_rows["issue"] = "duplicate_trip_stop_sequence"
        suspicious_parts.append(dup_seq_rows)
else:
    print("trip_id and/or stop_sequence missing.")


# =========================
# 13. Relationship sanity checks
# =========================
print_section("13. RELATIONSHIP SANITY CHECKS")

# Check if stop_id maps consistently to one stop name
if "stop_id" in dataset.columns and "stop_name" in dataset.columns:
    stop_name_map = dataset.groupby("stop_id")["stop_name"].nunique(dropna=True)
    inconsistent_stop_names = stop_name_map[stop_name_map > 1]
    print(f"stop_id values mapping to multiple stop_name values: {len(inconsistent_stop_names)}")

# Check if route_id maps consistently to one route_short_name
if "route_id" in dataset.columns and "route_short_name" in dataset.columns:
    route_map = dataset.groupby("route_id")["route_short_name"].nunique(dropna=True)
    inconsistent_routes = route_map[route_map > 1]
    print(f"route_id values mapping to multiple route_short_name values: {len(inconsistent_routes)}")

# Check if trip_id maps consistently to one shape_id
if "trip_id" in dataset.columns and "shape_id" in dataset.columns:
    shape_map = dataset.groupby("trip_id")["shape_id"].nunique(dropna=True)
    inconsistent_shapes = shape_map[shape_map > 1]
    print(f"trip_id values mapping to multiple shape_id values: {len(inconsistent_shapes)}")


# =========================
# 14. Low-information columns
# =========================
print_section("14. LOW-INFORMATION COLUMNS")

n_rows = len(dataset)
low_info = []

for col in dataset.columns:
    nunique = dataset[col].nunique(dropna=True)
    top_freq = dataset[col].value_counts(dropna=False, normalize=True).iloc[0]
    low_info.append(
        {
            "column": col,
            "n_unique": nunique,
            "top_frequency_ratio": round(float(top_freq), 4),
            "likely_constant": nunique <= 1,
            "dominant_value_over_95pct": top_freq > 0.95,
        }
    )

low_info_df = pd.DataFrame(low_info).sort_values(
    ["likely_constant", "dominant_value_over_95pct", "top_frequency_ratio"],
    ascending=[False, False, False],
)
print(low_info_df.to_string(index=False))
save_csv(low_info_df, "low_information_columns.csv")


# =========================
# 15. Optional route color parsing
# =========================
print_section("15. ROUTE COLOR SANITY")

if "route_color" in dataset.columns:
    rgb = dataset["route_color"].apply(parse_hex_color)
    dataset["route_color_r"] = rgb.apply(lambda x: x[0])
    dataset["route_color_g"] = rgb.apply(lambda x: x[1])
    dataset["route_color_b"] = rgb.apply(lambda x: x[2])

    invalid_color = dataset[
        dataset["route_color"].notna()
        & (
            dataset["route_color_r"].isna()
            | dataset["route_color_g"].isna()
            | dataset["route_color_b"].isna()
        )
    ].copy()

    print(f"Invalid route_color rows: {len(invalid_color)}")
    if len(invalid_color):
        invalid_color["issue"] = "invalid_route_color"
        suspicious_parts.append(invalid_color)
else:
    print("route_color not found.")


# =========================
# 16. Suspicious rows export
# =========================
print_section("16. EXPORT SUSPICIOUS ROWS")

if suspicious_parts:
    suspicious_df = pd.concat(suspicious_parts, ignore_index=True)
    suspicious_df = suspicious_df.drop_duplicates()
    print(f"Total suspicious rows collected: {len(suspicious_df)}")
    save_csv(suspicious_df, "suspicious_rows.csv")
else:
    print("No suspicious rows detected by current rules.")


# =========================
# 17. Final concise report
# =========================
print_section("17. FINAL REPORT")

summary_lines = []

summary_lines.append(f"Dataset shape: {dataset.shape}")
summary_lines.append(f"Total columns: {dataset.shape[1]}")
summary_lines.append(f"Duplicate rows: {dup_count}")

if TARGET_COL in dataset.columns:
    summary_lines.append(
        f"{TARGET_COL} missing: {int(dataset[TARGET_COL].isna().sum())}, "
        f"<=0: {int((dataset[TARGET_COL] <= 0).sum())}"
    )

if DIST_COL in dataset.columns:
    summary_lines.append(
        f"{DIST_COL} missing: {int(dataset[DIST_COL].isna().sum())}, "
        f"<=0: {int((dataset[DIST_COL] <= 0).sum())}"
    )

if "speed_kmh" in dataset.columns:
    summary_lines.append(
        f"speed_kmh > {MAX_SPEED_KMH}: {int((dataset['speed_kmh'] > MAX_SPEED_KMH).sum())}, "
        f"0 < speed_kmh < {MIN_SPEED_KMH}: {int(((dataset['speed_kmh'] > 0) & (dataset['speed_kmh'] < MIN_SPEED_KMH)).sum())}"
    )

if all(col in dataset.columns for col in FLAG_COLS):
    summary_lines.append(
        f"Invalid weekday flag rows: {int((dataset['day_flag_sum'] != 1).sum())}"
    )

for line in summary_lines:
    print(f"- {line}")

with open(OUTPUT_DIR / "validation_summary.txt", "w", encoding="utf-8") as f:
    for line in summary_lines:
        f.write(line + "\n")

print(f"\nValidation finished. Reports are in: {OUTPUT_DIR}")