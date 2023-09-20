# Alternative script, downloading National Wetland Inventory using ArcGIS REST API and query functionalities
# Developed this workflow for more efficiency and flexibility in data downloading

# But there is a limit to the amount of data you can download... default 1000 features :(
# Workaround: run initial query to return all object IDs within bounding box
# Then loop through objectIDs and append
# But there is also a limit to the length of the url... so made the list of objectids even smaller

import requests
import geopandas
import pandas
from shapely.geometry import shape
from fiona.crs import from_epsg
import os


def nwi_download_api(shed_gdf, out_dir, save=False):
    """
    Function to query NWI data from the ArcGIS Rest API service layer using a watershed bounding area.

    :param shed_gdf: watershed polygon geodataframe
    :param out_dir: location to save resulting wetland dataset
    :param save: if save is True, save output
    :return: geodataframe of wetlands
    """
    # url for ArcGIS REST service, wetland layer (which is 0)
    service_url = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0"

    # ensure input watershed shapefile is desired coordinate system (NAD83)
    check_crs = shed_gdf.crs
    target_crs = 'EPSG:4269'

    # if not desired CRS, convert to EPSG:4269
    if check_crs != target_crs:
        # Convert the GeoDataFrame to EPSG:4269
        shed_gdf = shed_gdf.to_crs(epsg=4269)
    else:
        print("CRS is already EPSG:4269.")

    # workflow: for the input watershed polygon geodataframe...
    # 1) download wetland objectids for input watershed polygon as geojson based on a bounding box
    # 2) iterate over returned subsets of objectids, and download wetland geometries based on the objectids
    # 3) aggregate wetland data into final geodataframe

    # get bounding box of watershed
    bounds_list = shed_gdf.total_bounds.tolist()
    # convert to string, with commas separating, for proper formatting
    bounds_string = [str(item) for item in bounds_list]
    bbox = ', '.join(bounds_string)
    print(bbox)

    # fill out query parameters, for NWI wetland map service
    query_params = {
        "f": "geojson",  # GeoJSON format for the response
        "geometry": bbox,  # bounding box
        "geometryType": "esriGeometryEnvelope",
        "spatialRel": "esriSpatialRelIntersects",
        "where": "1=1",  # Retrieve all features (modify as needed)
        "inSR": "4269",  # input spatial reference
        "outFields": "WETLAND_TYPE,ATTRIBUTE",  # specify fields of interest
        "returnIdsOnly": "true",
        # "outFields": "*",  # retrieve all fields
    }
    # create the API request URL
    api_url = f"{service_url}/query"

    # try sending the request, else return error information
    # code modified from ChatGPT-generated code
    # first sending request for Object IDs
    try:
        # send the API request
        response = requests.get(api_url, params=query_params)
        response.raise_for_status()  # Raise an exception for HTTP errors
        # parse the GeoJSON response
        geojson_data = response.json()

        # save the object IDs into list
        objectid_list = geojson_data['objectIds']
        # print(objectid_list)

    except requests.exceptions.RequestException as e:
        print(f"Request error: {e}")
    except Exception as e:
        print(f"An error occurred: {e}")

    # now run second request
    # this request is now for returning the featureids, looping through objectids (just doing 999 at a time, the limit)

    # define the request size
    # actually trying smaller request, seems to also be a limit on length of request url
    size = 99
    # create empty geodataframe to store the final dataset
    nwi_gdf_all = geopandas.GeoDataFrame()

    # loop through the object IDs in chunks
    for i in range(0, len(objectid_list), size):
        subset = objectid_list[i:i + size]

        # convert to string, with commas separating
        ids_string = [str(item) for item in subset]
        ids_final = ', '.join(ids_string)

        # try sending the second request, else return error information
        # fill out query parameters, for NWI wetland map service
        query_params_2 = {
            "f": "geojson",  # GeoJSON format for the response
            # "geometry": bbox,  # bounding box
            # "geometryType": "esriGeometryEnvelope",
            # "spatialRel": "esriSpatialRelIntersects",
            "where": "1=1",  # Retrieve all features (modify as needed)
            # "inSR": "4269",  # input spatial reference
            "outFields": "WETLAND_TYPE,ATTRIBUTE",  # specify fields of interest
            # "returnIdsOnly": "true",
            "objectIds": ids_final,
            # "outFields": "*",  # retrieve all fields
        }
        # create the API request URL
        api_url_2 = f"{service_url}/query"

        # try sending the request, else return error information
        # code modified from ChatGPT-generated code
        # first sending request for Object IDs
        try:
            # send the API request
            response_2 = requests.get(api_url_2, params=query_params_2)
            response_2.raise_for_status()  # Raise an exception for HTTP errors
            # parse the GeoJSON response
            geojson_data_2 = response_2.json()

            # Convert the GeoJSON data to a GeoDataFrame and specify CRS (EPSG:3857)
            geometries = [shape(feature["geometry"]) for feature in geojson_data_2["features"]]
            nwi_gdf = geopandas.GeoDataFrame(geojson_data_2["features"], geometry=geometries, crs=from_epsg(4326))

            # create new columns for the different fields, which is separating out the info in 'properties' field
            nwi_gdf["wet_type"] = [dict.get("WETLAND_TYPE") for dict in nwi_gdf["properties"]]
            nwi_gdf["attribute"] = [dict.get("ATTRIBUTE") for dict in nwi_gdf["properties"]]

            # aggregate the current results with the final dataset
            nwi_gdf_all = geopandas.GeoDataFrame(pandas.concat([nwi_gdf_all, nwi_gdf], ignore_index=True))

        except requests.exceptions.RequestException as e:
            print(f"Request error: {e}")
        except Exception as e:
            print(f"An error occurred: {e}")

    # convert to nad 83 coordinates
    nwi_gdf_final = nwi_gdf_all.to_crs(epsg=4269)

    if save:
        # output path for data
        file_name = 'nwi_wetlands.shp'
        # create pull file path
        file_path = os.path.join(out_dir, file_name)
        # save the GeoDataFrame as a shapefile
        nwi_gdf_final.to_file(file_path)
        print(f"Downloaded data and saved as {file_path}")
    else:
        print("GeoDataFrame not saved.")

    return nwi_gdf_final


# testing function
test_shed_gdf = geopandas.read_file("C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/camels_test_basin_3.shp")
test_nwi_out = nwi_download_api(shed_gdf=test_shed_gdf,
                                out_dir="C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data", save=True)

test_out_2 = test_nwi_out.overlay(test_shed_gdf, how='intersection')
print(test_out_2)

