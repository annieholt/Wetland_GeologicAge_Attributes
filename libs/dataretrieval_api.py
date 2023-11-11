# Alternative script, downloading datasets using ArcGIS REST API and other API query functionalities
# datasets: National Wetland Inventory, USGS daily discharge based on gageID
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

from datetime import datetime
import numpy
import scipy.io


def nwi_download_api(shed_gdf, out_dir, save=False):
    """
    Function to query NWI data from the ArcGIS Rest API service layer using a watershed bounding area.
    :param shed_gdf: watershed polygon geodataframe. should have 'gauge_id' attribute.
    :param out_dir: location to save resulting wetland dataset
    :param save: if save is True, save output
    :return: geodataframe of wetlands
    """
    # url for ArcGIS REST service, wetland layer (which is 0)
    service_url = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0"

    # ensure input watershed shapefile is desired coordinate system (NAD83)
    check_crs = shed_gdf.crs.to_string()
    target_crs = 'EPSG:4269'

    # if not desired CRS, convert to EPSG:4269
    if check_crs != target_crs:
        # Convert the GeoDataFrame to EPSG:4269
        shed_gdf = shed_gdf.to_crs(4269)
        # print(shed_gdf)
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

    # query_params = {
    #     "f": "geojson",  # GeoJSON format for the response
    #     "geometry": bbox,  # bounding box
    #     "geometryType": "esriGeometryEnvelope",
    #     "spatialRel": "esriSpatialRelIntersects",
    #     "where": "1=1",  # Retrieve all features (modify as needed)
    #     "inSR": "4269",  # input spatial reference
    #     # "outFields": "WETLAND_TYPE,ATTRIBUTE",  # specify fields of interest
    #     "returnIdsOnly": "true",
    #     # "outFields": "*",  # retrieve all fields
    # }

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
            # print(subset)

            # convert to string, with commas separating
            ids_string = [str(item) for item in subset]
            ids_final = ', '.join(ids_string)
            print(ids_final)

            # try sending the second request, else return error information
            # fill out query parameters, for NWI wetland map service
            query_params_2 = {
                "f": "geojson",  # GeoJSON format for the response
                # "geometry": bbox,  # bounding box
                # "geometryType": "esriGeometryEnvelope",
                # "spatialRel": "esriSpatialRelIntersects",
                "where": "1=1",  # Retrieve all features (modify as needed)
                # "inSR": "4269",  # input spatial reference
                "outFields": "Wetlands.WETLAND_TYPE,Wetlands.ATTRIBUTE",
                # specify fields of interest; somehow these changed??
                # "returnIdsOnly": "true",
                "objectIds": ids_final,
                # "outFields": "*",  # retrieve all fields
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
        # print(nwi_gdf_final)

        if save:
            # getting watershed id
            gauge_id = shed_gdf['gauge_id'].iloc[0]
            # output path for data
            file_name = gauge_id + '_nwi_wetlands.shp'
            # print(file_name)
            # create pull file path
            file_path = os.path.join(out_dir, file_name)
            # save the GeoDataFrame as a shapefile
            nwi_gdf_final.to_file(file_path)
            print(f"Downloaded data and saved as {file_path}")
        else:
            print("GeoDataFrame not saved.")

    except requests.exceptions.RequestException as e:
        print(f"Request error: {e}")
        # empty geodataframe
        return None
    except Exception as e:
        print(f"An error occurred: {e}")
        # empty geodataframe
        return None

    return nwi_gdf_final


# testing function
test_shed_gdf = geopandas.read_file("C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/camels_test_basin_2.shp")
# removing unneeded attribute data for now
test_shed_2 = test_shed_gdf.loc[:, ['hru_id', 'geometry']]
test_shed_2 = test_shed_2.rename(columns={'hru_id': 'gauge_id'})
test_shed_2['gauge_id'] = test_shed_2['gauge_id'].astype(str).str.zfill(8)

test_nwi_out = nwi_download_api(shed_gdf=test_shed_2,
                                out_dir="E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles", save=False)
print(test_nwi_out)

test_out_2 = test_nwi_out.overlay(test_shed_gdf, how='intersection')
print(test_out_2)


def usgs_daily_download_api(siteid, out_dir, save=False):
    """
    Function to download USGS daily discharge data, from USGS Daily Values Site Web REST Service
    :param siteid: site identifier, 8 digits long, as string
    :param out_dir: location to save dataset
    :param save: if true, save the dataset
    :return: dataframe with daily discharge values
    """

    # base URL of the USGS NWIS REST API
    base_url = "https://waterservices.usgs.gov/nwis/dv"

    # the parameters for the query
    # 1 Oct 1989 to 30 Sep 2009
    params = {
        "format": "json",
        "sites": siteid,
        "startDT": "1989-10-01",
        "endDT": "2009-09-30",
        "parameterCd": "00060"  # parameter code for daily discharge
    }

    try:
        # GET request to retrieve the data
        response = requests.get(base_url, params=params)
        response.raise_for_status()  # Raise an exception if there's an HTTP error

        # parse the JSON response
        data = response.json()

        # extract date and discharge values
        time_series = data["value"]["timeSeries"][0]["values"][0]["value"]

        # create a DataFrame of data
        q_df = pandas.DataFrame(time_series)
        # rename columns
        q_df.rename(columns={"dateTime": "date", "value": "q_daily_cfs"}, inplace=True)

        # convert date column to datetime format
        q_df["date"] = pandas.to_datetime(q_df["date"])

        print(q_df)

        if save:
            # create output file path
            file_name = siteid + ".csv"

            # create full file path
            file_path = os.path.join(out_dir, file_name)
            # save the GeoDataFrame as csv
            q_df.to_csv(file_path, index=False)
            print(f"Downloaded data and saved as {file_path}")
        else:
            print("Dataframe not saved.")

        return q_df

    except requests.exceptions.RequestException as e:
        print("Failed to retrieve data:", str(e))
    except Exception as e:
        print("An error occurred:", str(e))


def usgs_drain_area_download_api(siteid):
    """
    Function to download USGS daily discharge data, from USGS Daily Values Site Web REST Service.
    Based on function readNWISsite.R in dataRetrieval Package (https://github.com/DOI-USGS/dataRetrieval/blob/HEAD/R/readNWISsite.R).
    :param siteid: site identifier, 8 digits long, as string
    :return: a value of the basin drainage area
    """

    url = f"https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&format=rdb&sites={siteid}"
    # read data directly, which returns all the expanded site info
    data = pandas.read_csv(url, sep='\t', comment='#', header=0)

    # repalce '.' with NaN in columns containing '_va'
    va_columns = [col for col in data.columns if '_va' in col]
    data[va_columns] = data[va_columns].replace('.', pandas.NA)

    # Convert columns containing '_va' to numeric
    data[va_columns] = data[va_columns].apply(pandas.to_numeric, errors='coerce')

    # extract drainage area value from second row
    drain_area_va = data['drain_area_va'].iloc[1]

    # convert miles to mm2
    drain_area_mm2 = drain_area_va * (1609.34 ** 2) * (1000 ** 2)

    return drain_area_mm2


# should function return matlab file format instead??
def usgs_daily_prep(siteid, flow_cfs_df, drain_area, out_dir, save=False):
    """
    Function to prep USGS daily flow data for the TOSSH Toolbox. units are converted to mm/day, time series is filled
    out with NaNs, negative flow values are converted to NaN, date ranges are finalized.
    :param siteid: site identifier, 8 digits long, as string
    :param flow_cfs_df: dataframe of downloaded USGS flow data (columns 'date', 'qualifiers', 'q_daily_cfs')
    :param drain_area: value of drainage area for the site, in square milimeters
    :param out_dir: location to save dataset
    :return: dataframe of USGS discharge data, including flow values, dates, and qualifiers
    """

    flow_df = flow_cfs_df
    # Filtering data within the date range
    # making sure flow values are numeric format
    # flow_df = flow_df[(flow_df['date'] >= '1989-10-01') & (flow_df['date'] <= '2009-09-30')]
    flow_df['q_daily_cfs'] = pandas.to_numeric(flow_df['q_daily_cfs'], errors='coerce')

    # Calculating flow in mm/day, required for TOSSH
    flow_df['q_mm_day'] = flow_df['q_daily_cfs'] * 60 * 60 * 24 * (1 / 3.28084) ** 3 * (1000 ** 3) * (
            1 / drain_area)

    # Filling gaps with NaNs
    start_date = flow_df['date'].min()
    end_date = flow_df['date'].max()
    # print(start_date, end_date)

    if start_date.month >= 10:
        start_date_final = datetime(start_date.year, 10, 1)
    else:
        start_date_final = datetime(start_date.year - 1, 10, 1)

    if end_date.month >= 10:
        end_date_final = datetime(end_date.year + 1, 9, 30)
    else:
        end_date_final = datetime(end_date.year, 9, 30)

    # Convert the start and end dates back to the desired format ('%Y-%m-%d')
    start_date_str = start_date_final.strftime('%Y-%m-%d')
    end_date_str = end_date_final.strftime('%Y-%m-%d')

    date_range = pandas.date_range(start_date_str, end_date_str)
    # print(date_range)

    # Create a DataFrame with all dates in the range
    date_df = pandas.DataFrame({'date': date_range})

    # Merge the original data with the full date range DataFrame to fill gaps
    flow_df_full = date_df.merge(flow_df, on='date', how='left')
    # print(flow_df_full)

    # Replace negative Q.mm.day values with 0
    flow_df_full['q_mm_day'] = numpy.where(flow_df_full['q_mm_day'] < 0, 0, flow_df_full['q_mm_day'])

    # Format datetime
    flow_df_full['datetime'] = flow_df_full['date'].dt.strftime('%d-%b-%Y')

    # final columns for the export
    final_columns = ['datetime', 'q_mm_day', 'qualifiers']

    # Create a new DataFrame with only the specific columns
    flow_df_final = flow_df_full[final_columns]

    if save:
        # create output file path
        file_name = siteid + ".csv"

        # create full file path
        file_path = os.path.join(out_dir, file_name)
        # save the GeoDataFrame as csv
        flow_df_final.to_csv(file_path, index=False)
        print(f"Downloaded data and saved as {file_path}")
    else:
        print("Dataframe not saved.")

    return flow_df_final


# testing functions, on one USGS site
test_out = usgs_drain_area_download_api(siteid="02322800")
# test_df = usgs_daily_download_api(siteid="02322800",
#                                   out_dir="C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data", save=False)
test_df = usgs_daily_download_api(siteid="02322800",
                                  out_dir="C:/Users/aholt8450/Documents/Data/Gages_II", save=True)
# test_df_prepped = usgs_daily_prep(siteid="02322800", drain_area=test_out, flow_cfs_df=test_df,
#                                   out_dir="C:/Users/holta/Documents", save=True)
test_df_prepped = usgs_daily_prep(siteid="02322800", drain_area=test_out, flow_cfs_df=test_df,
                                  out_dir="C:/Users/aholt8450/Documents/Data/Gages_II_prepped", save=True)