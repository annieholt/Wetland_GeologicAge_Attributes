# ## scratch for NWI API workflows
#
# import requests
# import geopandas as gpd
# from shapely.geometry import shape
# from fiona.crs import from_epsg
#
# # Define the URL of the service and the specific layer you want to query
# service_url = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0"
#
# # Load your polygon shapefile representing the region of interest using GeoPandas
# # Replace 'region_shapefile.shp' with the path to your shapefile
# region_gdf = gpd.read_file("C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/camels_test_basin.shp")
#
# bbox_list = region_gdf.total_bounds.tolist()
# bbox = "-83.00255584756002, 30.18448257433115, -82.11312866178878, 31.254396438510064"
# print(bbox)
#
# # region_gdf_2 = region_gdf['geometry']
# # # Convert the polygon geometry to GeoJSON format
# # region_geojson = region_gdf_2.to_crs(epsg=4326).to_json()
# # print(region_geojson)
#
# # # Define the bounding box coordinates for EPSG:3857
# # bbox = "-13165545.81,3986470.65,-13054000.00,4075379.50"
#
# ids = [12582454, 12582458, 12582460]
#
# # convert to string, with commas separating
# ids_string = [str(item) for item in ids]
# ids_final = ', '.join(ids_string)
# print(ids_final)
#
# # Define query parameters
# query_params = {
#     "f": "geojson",  # Specify GeoJSON format for the response
#     # "geometry": bbox,  # Bounding box in EPSG:3857 format
#     # "geometryType": "esriGeometryEnvelope",
#     # "spatialRel": "esriSpatialRelIntersects",
#     "where": "1=1",  # Retrieve all features (modify as needed)
#     # "outFields": "*",  # Retrieve all fields (modify as needed)
#     # "inSR": "4326",
#     "outFields": "WETLAND_TYPE,ATTRIBUTE",
#     "objectIDs": ids_final,
# }
#
# # Construct the API request URL
# api_url = f"{service_url}/query"
#
# try:
#     # Send the API request
#     response = requests.get(api_url, params=query_params)
#     response.raise_for_status()  # Raise an exception for HTTP errors
#
#     # Parse the GeoJSON response
#     geojson_data = response.json()
#
#     # Convert the GeoJSON data to a GeoDataFrame and specify CRS (EPSG:3857)
#     geometries = [shape(feature["geometry"]) for feature in geojson_data["features"]]
#     gdf = gpd.GeoDataFrame(geojson_data["features"], geometry=geometries, crs=from_epsg(4326))
#     gdf["wetland_types"] = [dict.get("WETLAND_TYPE") for dict in gdf["properties"]]
#     gdf["attribute"] = [dict.get("ATTRIBUTE") for dict in gdf["properties"]]
#
#     # Define the output shapefile path (modify as needed)
#     output_shapefile_path = "wetlands_shapefile.shp"
#
#     # Save the GeoDataFrame as a shapefile
#     gdf.to_file(output_shapefile_path)
#
#     print(f"Downloaded data and saved as {output_shapefile_path}")
#
# except requests.exceptions.RequestException as e:
#     print(f"Request error: {e}")
# except Exception as e:
#     print(f"An error occurred: {e}")


import requests
import geopandas
import pandas
from shapely.geometry import shape
from fiona.crs import from_epsg


# testing function
test_shed_gdf = geopandas.read_file("C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/camels_test_basin_2.shp")
# removing unneeded attribute data for now
test_shed_2 = test_shed_gdf.loc[:, ['hru_id', 'geometry']]
test_shed_2 = test_shed_2.rename(columns={'hru_id': 'gauge_id'})
test_shed_2['gauge_id'] = test_shed_2['gauge_id'].astype(str).str.zfill(8)

# url for ArcGIS REST service, wetland layer (which is 0)
service_url = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0"

# ensure input watershed shapefile is desired coordinate system (NAD83)
check_crs = test_shed_2.crs.to_string()
target_crs = 'EPSG:4269'

# if not desired CRS, convert to EPSG:4269
if check_crs != target_crs:
    # Convert the GeoDataFrame to EPSG:4269
    shed_gdf = test_shed_2.to_crs(4269)
    # print(shed_gdf)
else:
    print("CRS is already EPSG:4269.")

# workflow: for the input watershed polygon geodataframe...
# 1) download wetland objectids for input watershed polygon as geojson based on a bounding box
# 2) iterate over returned subsets of objectids, and download wetland geometries based on the objectids
# 3) aggregate wetland data into final geodataframe

# get bounding box of watershed
bounds_list = test_shed_2.total_bounds.tolist()
# convert to string, with commas separating, for proper formatting
bounds_string = [str(item) for item in bounds_list]
bbox = ', '.join(bounds_string)
print(bbox)

# fill out query parameters, for NWI wetland map service
# Define the query parameters as a dictionary
# below is an updated version, kept previous version; not sure why this needed to change?
query_params = {
    "where": "1=1",
    "text": "",
    "objectIds": "",
    "time": "",
    "geometry": bbox,
    "geometryType": "esriGeometryEnvelope",
    "inSR": "4269",
    "spatialRel": "esriSpatialRelIntersects",
    "units": "esriSRUnit_Foot",
    "returnIdsOnly": "true",
    "f": "pjson"
}

# create the API request URL
api_url = f"{service_url}/query"
# print(api_url)

# try sending the request, else return error information
# code modified from ChatGPT-generated code
# first sending request for Object IDs

try:
    # send the API request
    response = requests.get(api_url, params=query_params)
    response.raise_for_status()  # Raise an exception for HTTP errors
    # parse the GeoJSON response
    geojson_data = response.json()
    print(geojson_data)

    # save the object IDs into the list
    objectid_list = geojson_data['objectIds']
    print(objectid_list)

    # now run second request
    # this request is now for returning the featureids,
    # looping through objectids (just doing 99 at a time, the limit)

    # define the request size
    # actually trying smaller request, seems to also be a limit on length of request url
    size = 99
    # create empty geodataframe to store the final dataset
    nwi_gdf_all = geopandas.GeoDataFrame()

    # loop through the object IDs in chunks
    for i in range(0, len(objectid_list), size):
        subset = objectid_list[i:i + size]
        print(subset)

        # convert to string, with commas separating
        ids_string = [str(item) for item in subset]
        ids_final = ', '.join(ids_string)
        print(ids_final)

        # try sending the second request, else return error information
        # fill out query parameters, for NWI wetland map service
        # query_params_2 = {
        #     "f": "geojson",  # GeoJSON format for the response
        #     # "geometry": bbox,  # bounding box
        #     # "geometryType": "esriGeometryEnvelope",
        #     # "spatialRel": "esriSpatialRelIntersects",
        #     "where": "1=1",  # Retrieve all features (modify as needed)
        #     # "inSR": "4269",  # input spatial reference
        #     "outFields": "Wetlands.WETLAND_TYPE,Wetlands.ATTRIBUTE",
        #     # specify fields of interest; somehow these changed??
        #     # "returnIdsOnly": "true",
        #     "objectIds": ids_final,
        #     # "outFields": "*",  # retrieve all fields
        # }

        query_params_2 = {
            "where": "1=1",
            "text": "",
            "objectIds": ids_final,
            "time": "",
            "geometry": "",
            "geometryType": "esriGeometryEnvelope",
            "inSR": "",
            "spatialRel": "esriSpatialRelIntersects",
            "distance": "",
            "units": "esriSRUnit_Foot",
            "relationParam": "",
            "outFields": "",
            "returnGeometry": "true",
            "returnTrueCurves": "false",
            "maxAllowableOffset": "",
            "geometryPrecision": "",
            "outSR": "",
            "havingClause": "",
            "returnIdsOnly": "false",
            "returnCountOnly": "false",
            "orderByFields": "",
            "groupByFieldsForStatistics": "",
            "outStatistics": "",
            "returnZ": "false",
            "returnM": "false",
            "gdbVersion": "",
            "historicMoment": "",
            "returnDistinctValues": "false",
            "resultOffset": "",
            "resultRecordCount": "",
            "returnExtentOnly": "false",
            "datumTransformation": "",
            "parameterValues": "",
            "rangeValues": "",
            "quantizationParameters": "",
            "featureEncoding": "esriDefault",
            "f": "geojson"
        }

        # create the API request URL
        api_url_2 = f"{service_url}/query"

        # send the API request
        response_2 = requests.get(api_url_2, params=query_params_2)
        response_2.raise_for_status()  # Raise an exception for HTTP errors
        # parse the GeoJSON response
        geojson_data_2 = response_2.json()
        print(geojson_data_2)

        # Convert the GeoJSON data to a GeoDataFrame and specify CRS (EPSG:3857)
        geometries = [shape(feature["geometry"]) for feature in geojson_data_2["features"]]
        nwi_gdf = geopandas.GeoDataFrame(geojson_data_2["features"], geometry=geometries, crs=from_epsg(4326))

        # create new columns for the different fields, which is separating out the info in 'properties' field
        # had to update the field names, leaving old version below for now

        # nwi_gdf["wet_type"] = [dict.get("WETLAND_TYPE") for dict in nwi_gdf["properties"]]
        # nwi_gdf["attribute"] = [dict.get("ATTRIBUTE") for dict in nwi_gdf["properties"]]

        nwi_gdf["attribute"] = [dict.get("Wetlands.ATTRIBUTE") for dict in nwi_gdf["properties"]]
        nwi_gdf["wet_type"] = [dict.get("Wetlands.WETLAND_TYPE") for dict in nwi_gdf["properties"]]

        # drop columns to reduce dataset size
        nwi_gdf = nwi_gdf.drop(columns=['type', 'properties'])

        # aggregate the current results with the final dataset
        nwi_gdf_all = geopandas.GeoDataFrame(pandas.concat([nwi_gdf_all, nwi_gdf], ignore_index=True))

    # convert to nad 83 coordinates
    nwi_gdf_final = nwi_gdf_all.to_crs(4269)
    print(nwi_gdf_final)

except requests.exceptions.RequestException as e:
    print(f"Request error: {e}")
except Exception as e:
    print(f"An error occurred: {e}")









# # now run second request
#     # this request is now for returning the featureids, looping through objectids (just doing 999 at a time, the limit)
#
#     # define the request size
#     # actually trying smaller request, seems to also be a limit on length of request url
#     size = 99
#     # create empty geodataframe to store the final dataset
#     nwi_gdf_all = geopandas.GeoDataFrame()
#
#     # loop through the object IDs in chunks
#     for i in range(0, len(objectid_list), size):
#         subset = objectid_list[i:i + size]
#         # print(subset)
#
#         # convert to string, with commas separating
#         ids_string = [str(item) for item in subset]
#         ids_final = ', '.join(ids_string)
#
#         # try sending the second request, else return error information
#         # fill out query parameters, for NWI wetland map service
#         query_params_2 = {
#             "f": "geojson",  # GeoJSON format for the response
#             # "geometry": bbox,  # bounding box
#             # "geometryType": "esriGeometryEnvelope",
#             # "spatialRel": "esriSpatialRelIntersects",
#             "where": "1=1",  # Retrieve all features (modify as needed)
#             # "inSR": "4269",  # input spatial reference
#             "outFields": "Wetlands.WETLAND_TYPE,Wetlands.ATTRIBUTE",  # specify fields of interest; somehow these changed??
#             # "returnIdsOnly": "true",
#             "objectIds": ids_final,
#             # "outFields": "*",  # retrieve all fields
#         }
#         # create the API request URL
#         api_url_2 = f"{service_url}/query"
#
#         # try sending the request, else return error information
#         # code modified from ChatGPT-generated code
#         # first sending request for Object IDs
#         try:
#             # send the API request
#             response_2 = requests.get(api_url_2, params=query_params_2)
#             response_2.raise_for_status()  # Raise an exception for HTTP errors
#             # parse the GeoJSON response
#             geojson_data_2 = response_2.json()
#             # print(geojson_data_2)
#
#             # Convert the GeoJSON data to a GeoDataFrame and specify CRS (EPSG:3857)
#             geometries = [shape(feature["geometry"]) for feature in geojson_data_2["features"]]
#             nwi_gdf = geopandas.GeoDataFrame(geojson_data_2["features"], geometry=geometries, crs=from_epsg(4326))
#
#             # create new columns for the different fields, which is separating out the info in 'properties' field
#             # had to update the field names, leaving old version below for now
#
#             # nwi_gdf["wet_type"] = [dict.get("WETLAND_TYPE") for dict in nwi_gdf["properties"]]
#             # nwi_gdf["attribute"] = [dict.get("ATTRIBUTE") for dict in nwi_gdf["properties"]]
#
#             nwi_gdf["attribute"] = [dict.get("Wetlands.ATTRIBUTE") for dict in nwi_gdf["properties"]]
#             nwi_gdf["wet_type"] = [dict.get("Wetlands.WETLAND_TYPE") for dict in nwi_gdf["properties"]]
#
#             # drop columns to reduce dataset size
#             nwi_gdf = nwi_gdf.drop(columns=['type', 'properties'])
#
#             # aggregate the current results with the final dataset
#             nwi_gdf_all = geopandas.GeoDataFrame(pandas.concat([nwi_gdf_all, nwi_gdf], ignore_index=True))
#
#         except requests.exceptions.RequestException as e:
#             print(f"Request error: {e}")
#         except Exception as e:
#             print(f"An error occurred: {e}")
#
#     # convert to nad 83 coordinates
#     nwi_gdf_final = nwi_gdf_all.to_crs(4269)
#     # print(nwi_gdf_final)
#
#     if save:
#         # getting watershed id
#         gauge_id = shed_gdf['gauge_id'].iloc[0]
#         # output path for data
#         file_name = gauge_id + '_nwi_wetlands.shp'
#         # print(file_name)
#         # create pull file path
#         file_path = os.path.join(out_dir, file_name)
#         # save the GeoDataFrame as a shapefile
#         nwi_gdf_final.to_file(file_path)
#         print(f"Downloaded data and saved as {file_path}")
#     else:
#         print("GeoDataFrame not saved.")
#
#     return nwi_gdf_final








### Scratch for Gages II API workflows

import requests
import pandas as pd

# # Define the base URL of the USGS NWIS REST API
# base_url = "https://waterservices.usgs.gov/nwis/dv"
#
# # Define the parameters for the query
# params = {
#     "format": "json",
#     "sites": "02322800",
#     "startDT": "2022-01-01",
#     "endDT": "2022-12-31",
#     "parameterCd": "00060"  # Parameter code for daily discharge
# }
#
# try:
#     # Make the GET request to retrieve the data
#     response = requests.get(base_url, params=params)
#     response.raise_for_status()  # Raise an exception if there's an HTTP error
#
#     # Parse the JSON response
#     data = response.json()
#
#     # Extract date and discharge values
#     time_series = data["value"]["timeSeries"][0]["values"][0]["value"]
#
#     # Create a DataFrame from the extracted data
#     df = pd.DataFrame(time_series)
#
#     # Rename columns for clarity
#     df.rename(columns={"dateTime": "Date", "value": "Discharge (cfs)"}, inplace=True)
#
#     # Convert Date column to datetime format
#     df["Date"] = pd.to_datetime(df["Date"])
#
#     # Now, you have the data in a DataFrame, and you can work with it as needed
#     print(df)
#
# except requests.exceptions.RequestException as e:
#     print("Failed to retrieve data:", str(e))
# except Exception as e:
#     print("An error occurred:", str(e))

