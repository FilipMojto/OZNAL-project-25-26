import pandas as pd

DATASETS_DIR = "data/data_bratislava_mhd_timetable_gtfs/Datasets/"

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

columns = [
    "trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence",
    "stop_headsign", "pickup_type", "drop_off_type", "route_id", "service_id",
    "trip_headsign", "direction_id", "shape_id", "wheelchair_accessible",
    "stop_code", "stop_name", "stop_lat", "stop_lon", "zone_id", "platform_code",
    "zone_name", "monday", "tuesday", "wednesday", "thursday", "friday",
    "saturday", "sunday", "start_date", "end_date", "route_short_name",
    "route_long_name", "route_type", "route_color"
]
dataset = dataset[columns]

dataset.to_csv("dataset.csv", index=False)
print("Saved to dataset.csv")