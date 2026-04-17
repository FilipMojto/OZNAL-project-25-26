import os
from pathlib import Path

import pandas as pd
import numpy as np
from dotenv import load_dotenv

from config import DATASETS_DIR, INTERIM_DIR
# DATASETS_DIR = "data/Datasets/"

# load environment variable from .env file

OUTPUT_FILE = INTERIM_DIR + "dataset.csv"

Path(INTERIM_DIR).mkdir(parents=True, exist_ok=True)

stop_times = pd.read_csv(DATASETS_DIR + "stop_times.txt", low_memory=False)
print(f"Stop times: {len(stop_times)} rows")

trips = pd.read_csv(DATASETS_DIR + "trips.txt")
print(f"Trips: {len(trips)} rows")

stops = pd.read_csv(DATASETS_DIR + "stops.txt", low_memory=False)
print(f"Stops: {len(stops)} rows")

tarifni_zony = pd.read_csv(DATASETS_DIR + "Tarifni_zony.txt")
print(f"Tarifni zony: {len(tarifni_zony)} rows")

calendar = pd.read_csv(DATASETS_DIR + "calendar.txt")
print(f"Calendar: {len(calendar)} rows")

routes = pd.read_csv(DATASETS_DIR + "routes.txt")
print(f"Routes: {len(routes)} rows")

agency = pd.read_csv(DATASETS_DIR + "agency.txt")
print(f"Agency: {len(agency)} rows")

dataset = stop_times.merge(trips, on="trip_id", how="left")
dataset = dataset.merge(stops, on="stop_id", how="left")
tarifni_zony["zone_id"] = tarifni_zony["zone_id"].astype(str)
dataset["zone_id"] = dataset["zone_id"].astype(str)
dataset = dataset.merge(tarifni_zony, on="zone_id", how="left")
dataset = dataset.merge(calendar, on="service_id", how="left")
dataset = dataset.merge(routes, on="route_id", how="left")
dataset = dataset.merge(agency, on="agency_id", how="left")
print(f"Dataset: {len(dataset)} rows")


# --- Segment time (seconds) ---
def time_to_seconds(t):
    """Converts HH:MM:SS to seconds, supports times beyond 24h (e.g. 25:10:00)."""
    h, m, s = map(int, t.split(":"))
    return h * 3600 + m * 60 + s

dataset = dataset.sort_values(["trip_id", "stop_sequence"]).reset_index(drop=True)

dataset["arrival_seconds"] = dataset["arrival_time"].apply(time_to_seconds)
dataset["departure_seconds"] = dataset["departure_time"].apply(time_to_seconds)

next_arrival = dataset.groupby("trip_id")["arrival_seconds"].shift(-1)
dataset["segment_time_s"] = next_arrival - dataset["departure_seconds"]

# lets print rows where segment_time_s == 0 or negative, which could indicate data issues or very short segments
print("Rows with non-positive segment times:")
print(dataset[dataset["segment_time_s"] <= 0][["trip_id", "stop_id", "arrival_time", "departure_time", "segment_time_s"]])

# --- Segment distance (meters) via shape_dist_traveled from shapes.txt ---
def haversine(lat1, lon1, lat2, lon2):
    R = 6371000
    phi1, phi2 = np.radians(lat1), np.radians(lat2)
    dphi = np.radians(lat2 - lat1)
    dlambda = np.radians(lon2 - lon1)
    a = np.sin(dphi / 2) ** 2 + np.cos(phi1) * np.cos(phi2) * np.sin(dlambda / 2) ** 2
    return R * 2 * np.arcsin(np.sqrt(a))

shapes = pd.read_csv(DATASETS_DIR + "shapes.txt")
shapes = shapes.sort_values(["shape_id", "shape_pt_sequence"]).reset_index(drop=True)

# Compute cumulative distance along each shape
shapes["d_prev"] = haversine(
    shapes["shape_pt_lat"], shapes["shape_pt_lon"],
    shapes.groupby("shape_id")["shape_pt_lat"].shift(1),
    shapes.groupby("shape_id")["shape_pt_lon"].shift(1),
)
shapes["shape_dist_traveled"] = shapes.groupby("shape_id")["d_prev"].cumsum().fillna(0)

# For each stop in a trip, find the nearest shape point and get its cumulative distance
# shape_id is already in dataset from the trips merge above
def get_shape_dist(group):
    """For each stop in a trip, find the nearest shape point by lat/lon and return its shape_dist_traveled."""
    shape_id = group["shape_id"].iloc[0]
    shape_pts = shapes[shapes["shape_id"] == shape_id][
        ["shape_pt_lat", "shape_pt_lon", "shape_dist_traveled"]
    ].reset_index(drop=True)

    dists = haversine(
        group["stop_lat"].values[:, None],
        group["stop_lon"].values[:, None],
        shape_pts["shape_pt_lat"].values[None, :],
        shape_pts["shape_pt_lon"].values[None, :],
    )
    nearest_idx = dists.argmin(axis=1)
    return shape_pts.loc[nearest_idx, "shape_dist_traveled"].values

dataset["stop_shape_dist"] = np.concatenate(
    [get_shape_dist(g) for _, g in dataset.groupby("trip_id", sort=False)]
)

next_shape_dist = dataset.groupby("trip_id")["stop_shape_dist"].shift(-1)
dataset["segment_distance_m"] = next_shape_dist - dataset["stop_shape_dist"]

dataset.rename(columns={"stop_shape_dist": "shape_dist_traveled"}, inplace=True)

# --- Arrival time components ---
arrival_parts = dataset["arrival_time"].str.split(":", expand=True).astype(int)
dataset["arrival_time_hour"]   = arrival_parts[0] % 24   # normalize past-midnight hours
dataset["arrival_time_minute"] = arrival_parts[1]
dataset["arrival_time_second"] = arrival_parts[2]
dataset["seconds_since_midnight"] = dataset["arrival_seconds"] % 86400

# --- Day-type flags ---
# dataset["is_weekday"]  = (dataset[["monday", "tuesday", "wednesday", "thursday", "friday"]].max(axis=1)).astype(int)
# dataset["is_saturday"] = dataset["saturday"].astype(int)
# dataset["is_sunday"]   = dataset["sunday"].astype(int)
# updated to single categorical column instead of 3 binary flags
dataset["day_type"] = np.select(
    [
        # runs all week
        (dataset["monday"] == 1) &
        (dataset["tuesday"] == 1) &
        (dataset["wednesday"] == 1) &
        (dataset["thursday"] == 1) &
        (dataset["friday"] == 1) &
        (dataset["saturday"] == 1) &
        (dataset["sunday"] == 1),

        # saturday only
        (dataset["saturday"] == 1) &
        (dataset[["monday","tuesday","wednesday","thursday","friday","sunday"]].sum(axis=1) == 0),

        # sunday only
        (dataset["sunday"] == 1) &
        (dataset[["monday","tuesday","wednesday","thursday","friday","saturday"]].sum(axis=1) == 0),
    ],
    [
        "all_week",
        "saturday",
        "sunday"
    ],
    default="weekday"
)

# --- Time since last stop (seconds between prev departure and current arrival) ---
prev_departure = dataset.groupby("trip_id")["departure_seconds"].shift(1)
dataset["time_since_last_stop"] = dataset["arrival_seconds"] - prev_departure

print(f"Segment time nulls:     {dataset['segment_time_s'].isna().sum()} (last stops in each trip)")
print(f"Segment distance nulls: {dataset['segment_distance_m'].isna().sum()} (last stops in each trip)")
print(f"Time since last nulls:  {dataset['time_since_last_stop'].isna().sum()} (first stops in each trip)")

# --- data cleaning steps ---
# dropping the last stops
dataset = dataset[dataset["segment_time_s"].notna()]

dataset.to_csv(OUTPUT_FILE, index=False)
print(f"Saved to {OUTPUT_FILE}")