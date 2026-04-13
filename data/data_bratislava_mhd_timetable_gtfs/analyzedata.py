import pandas as pd

# now let's read the data from the txt file
# there is a header row, and the values are separated by commas
data = pd.read_csv('./shapes.txt', sep=',')

print(data.info())

print(data.head())

# now let's analyze missing values of the data
missing_values = data.isnull().sum()
print("Missing values per column:")
print(missing_values)

# # let's also count the occurrences of each pickup type
# pickup_type_counts = data['pickup_type'].value_counts()
# print("Pickup type counts:")
# print(pickup_type_counts)

# # let's analyze same for dropoff type
# dropoff_type_counts = data['drop_off_type'].value_counts()
# print("Dropoff type counts:")
# print(dropoff_type_counts)

# let's analyze the same for zone_ids
# zone_id_counts = data['zone_id'].value_counts()
# print("Zone ID counts:")
# print(zone_id_counts)

# #let's also analyze the same for location_type
# location_type_counts = data['location_type'].value_counts()
# print("Location type counts:")
# print(location_type_counts)

# # let's analyze the same for platform_code
# platform_code_counts = data['platform_code'].value_counts()
# print("Platform code counts:")
# print(platform_code_counts)

# let's analyze the same for direction_id
# direction_id_counts = data['direction_id'].value_counts()
# print("Direction ID counts:")
# print(direction_id_counts)

# # let's analyze the same for wheelchair_accessible
# wheelchair_accessible_counts = data['wheelchair_accessible'].value_counts()
# print("Wheelchair accessible counts:")
# print(wheelchair_accessible_counts)

# let's analyze the same for route_type
# route_type_counts = data['route_type'].value_counts()
# print("Route type counts:")
# print(route_type_counts)

# # let's analyze the same for route_color
# route_color_counts = data['route_color'].value_counts()
# print("Route color counts:")
# print(route_color_counts)

# # let's analyze the same for agency_id
# agency_id_counts = data['agency_id'].value_counts()
# print("Agency ID counts:")
# print(agency_id_counts)

# let's analyze the same for service_id
# service_id_counts = data['service_id'].value_counts()
# print("Service ID counts:")
# print(service_id_counts)