# Alternative script, downloading National Wetland Inventory using ArcGIS REST API and query functionalities
# Developed this workflow for more efficiency and flexibility in data downloading

import requests
import geopandas as gpd
from shapely.geometry import shape
from fiona.crs import from_epsg

# Define the URL of the service and the specific layer you want to query
service_url = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0"

# Define the bounding box coordinates for EPSG:3857
bbox = "-13165545.81,3986470.65,-13054000.00,4075379.50"

# Define query parameters
query_params = {
    "f": "geojson",  # Specify GeoJSON format for the response
    "geometry": bbox,  # Bounding box in EPSG:3857 format
    "geometryType": "esriGeometryEnvelope",
    "spatialRel": "esriSpatialRelIntersects",
    "where": "1=1",  # Retrieve all features (modify as needed)
    # "outFields": "*",  # Retrieve all fields (modify as needed)
    "inSR": "3857",
    "outFields": "WETLAND_TYPE,ATTRIBUTE",
}

# Construct the API request URL
api_url = f"{service_url}/query"

try:
    # Send the API request
    response = requests.get(api_url, params=query_params)
    response.raise_for_status()  # Raise an exception for HTTP errors

    # Parse the GeoJSON response
    geojson_data = response.json()

    # Convert the GeoJSON data to a GeoDataFrame and specify CRS (EPSG:3857)
    geometries = [shape(feature["geometry"]) for feature in geojson_data["features"]]
    gdf = gpd.GeoDataFrame(geojson_data["features"], geometry=geometries, crs=from_epsg(4326))
    gdf["wetland_types"] = [dict.get("WETLAND_TYPE") for dict in gdf["properties"]]
    gdf["attribute"] = [dict.get("ATTRIBUTE") for dict in gdf["properties"]]

    # Define the output shapefile path (modify as needed)
    output_shapefile_path = "wetlands_shapefile.shp"

    # Save the GeoDataFrame as a shapefile
    gdf.to_file(output_shapefile_path)

    print(f"Downloaded data and saved as {output_shapefile_path}")

except requests.exceptions.RequestException as e:
    print(f"Request error: {e}")
except Exception as e:
    print(f"An error occurred: {e}")