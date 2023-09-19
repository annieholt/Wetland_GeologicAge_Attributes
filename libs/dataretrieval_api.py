# Alternative script, downloading National Wetland Inventory using ArcGIS REST API and query functionalities
# Developed this workflow for more efficiency and flexibility in data downloading

import requests
import geopandas
from shapely.geometry import shape
from fiona.crs import from_epsg
import os


def nwi_download_api(shed_gdf, out_dir):
    """
    :param shed_gdf: geodataframe of watershed polygons
    :param out_dir: location to save resulting wetland dataset
    :return: geodataframe of wetlands
    """
    # url for ArcGIS REST service, wetland layer (which is 0)
    service_url = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0"

    # ensure input watershed shapefile is desired coordinate system (WGS84)
    check_crs = shed_gdf.crs
    target_crs = 'EPSG:4326'

    # if not desired CRS, convert to EPSG:4326
    if check_crs != target_crs:
        # Convert the GeoDataFrame to EPSG:4326
        shed_gdf = shed_gdf.to_crs(target_crs)
    else:
        print("CRS is already EPSG:4326.")

    # for every watershed polygon in geodataframe:
    # 1) download wetland shapefiles based on a bounding box
    # 2) clip wetland geodataframe to exact polygon of watershed
    # 3) aggregate watersheds to one final dataset
    # 4) save dataset to output directory

    # list of wetland data results
    wetland_out = []

    # iterate over every feature in watershed geodataframe
    for index, row in shed_gdf.iterrows():
        # get bounding box from watershed polygon, as string
        bounds_list = row['geometry'].bounds
        print(bounds_list)
        bbox = str(bounds_list)

        # fill out query parameters, for NWI wetland map service
        query_params = {
            "f": "geojson",  # GeoJSON format for the response
            "geometry": bbox,  # bounding box
            "geometryType": "esriGeometryEnvelope",
            "spatialRel": "esriSpatialRelIntersects",
            "where": "1=1",  # Retrieve all features (modify as needed)
            "inSR": "4326",  # input spatial reference
            "outFields": "WETLAND_TYPE,ATTRIBUTE",  # specify fields of interest
            # "outFields": "*",  # retrieve all fields
        }
        # create the API request URL
        api_url = f"{service_url}/query"

        # try sending the request, else return error information
        # code modified from ChatGPT-generated code
        try:
            # send the API request
            response = requests.get(api_url, params=query_params)
            response.raise_for_status()  # Raise an exception for HTTP errors
            # parse the GeoJSON response
            geojson_data = response.json()

            # Convert the GeoJSON data to a GeoDataFrame and specify CRS (EPSG:3857)
            geometries = [shape(feature["geometry"]) for feature in geojson_data["features"]]
            nwi_gdf = geopandas.GeoDataFrame(geojson_data["features"], geometry=geometries, crs=from_epsg(4326))

            # create new columns for the different fields, which is separating out the info in 'properties' field
            nwi_gdf["wetland_types"] = [dict.get("WETLAND_TYPE") for dict in nwi_gdf["properties"]]
            nwi_gdf["attribute"] = [dict.get("ATTRIBUTE") for dict in nwi_gdf["properties"]]

            wetland_out.append(nwi_gdf)

        except requests.exceptions.RequestException as e:
            print(f"Request error: {e}")
        except Exception as e:
            print(f"An error occurred: {e}")

    # Concatenate the list of resulting features into a final GeoDataFrame
    final_gdf = geopandas.GeoDataFrame(wetland_out)

    # output path for data
    file_name = 'nwi_wetlands.shp'
    # create pull file path
    file_path = os.path.join(out_dir, file_name)

    # save the GeoDataFrame as a shapefile
    final_gdf.to_file(file_path)
    print(f"Downloaded data and saved as {file_path}")

    return final_gdf


# testing function
test_gdf = geopandas.read_file("C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/camels_test_basin_2.shp")

test_out = nwi_download_api(shed_gdf=test_gdf, out_dir="C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data")

print(test_out)