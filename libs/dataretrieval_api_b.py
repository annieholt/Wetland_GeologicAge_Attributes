import geopandas
from shapely.geometry import shape
from pyproj import CRS
import pandas
import requests


def convert_to_epsg(shed_gdf, target_epsg):
    check_crs = shed_gdf.crs.to_string()
    if check_crs != target_epsg:
        return shed_gdf.to_crs(target_epsg)
    else:
        print("CRS is already {}".format(target_epsg))
        return shed_gdf


def get_object_ids(api_url, query_params):
    try:
        response = requests.get(api_url, params=query_params)
        response.raise_for_status()
        geojson_data = response.json()
        print(geojson_data)
        return geojson_data['objectIds']
    except requests.exceptions.RequestException as e:
        print("Request error:", e)
        print("Response content:", response.content)
        return []
    except Exception as e:
        print("An error occurred:", e)
        return []


def get_wetland_data(api_url, query_params_2, objectid_list):
    size = 99
    nwi_gdf_all = geopandas.GeoDataFrame()

    for i in range(0, len(objectid_list), size):
        subset = objectid_list[i:i + size]
        ids_final = ', '.join(map(str, subset))

        query_params_2['objectIds'] = ids_final
        try:
            response_2 = requests.get(api_url, params=query_params_2)
            response_2.raise_for_status()
            geojson_data_2 = response_2.json()

            geometries = [shape(feature["geometry"]) for feature in geojson_data_2["features"]]
            nwi_gdf = geopandas.GeoDataFrame(geojson_data_2["features"], geometry=geometries, crs=CRS.from_epsg(4326))

            nwi_gdf["attribute"] = [dict.get("Wetlands.ATTRIBUTE") for dict in nwi_gdf["properties"]]
            nwi_gdf["wet_type"] = [dict.get("Wetlands.WETLAND_TYPE") for dict in nwi_gdf["properties"]]

            nwi_gdf = nwi_gdf.drop(columns=['type', 'properties'])

            nwi_gdf_all = geopandas.GeoDataFrame(pandas.concat([nwi_gdf_all, nwi_gdf], ignore_index=True))

        except requests.exceptions.RequestException as e:
            print("Request error:", e)
        except Exception as e:
            print("An error occurred:", e)

    return nwi_gdf_all


if __name__ == "__main__":
    # Load test data
    # test_shed_gdf = geopandas.read_file(
    #     "C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/camels_test_basin_2.shp")

    test_shed_gdf = geopandas.read_file(
        "C:/Users/aholt8450/Documents/Data/camels_test_basin.shp")

    # Preprocess test data
    # test_shed_2 = test_shed_gdf.loc[:, ['hru_id', 'geometry']].rename(columns={'hru_id': 'gauge_id'}).astype(
    #     {'gauge_id': str}).str.zfill(8)

    # removing unneeded attribute data for now
    test_shed_2 = test_shed_gdf.loc[:, ['hru_id', 'geometry']]
    test_shed_2 = test_shed_2.rename(columns={'hru_id': 'gauge_id'})
    test_shed_2['gauge_id'] = test_shed_2['gauge_id'].astype(str).str.zfill(8)

    # Set target CRS
    target_crs = 'EPSG:4269'
    shed_gdf = convert_to_epsg(test_shed_2, target_crs)
    print(shed_gdf)

    # Define API URL and query parameters
    service_url = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0"
    bbox = ', '.join(map(str, shed_gdf.total_bounds.tolist()))
    print(bbox)
    query_params = {"where": "1=1", "text": "", "objectIds": "", "time": "", "geometry": bbox,
                    "geometryType": "esriGeometryEnvelope", "inSR": "4269", "spatialRel": "esriSpatialRelIntersects",
                    "units": "esriSRUnit_Foot", "returnIdsOnly": "true", "f": "pjson"}
    query_params_2 = {"where": "1=1", "text": "", "objectIds": "", "time": "", "geometry": "",
                      "geometryType": "esriGeometryEnvelope", "inSR": "", "spatialRel": "esriSpatialRelIntersects",
                      "distance": "", "units": "esriSRUnit_Foot", "relationParam": "", "outFields": "",
                      "returnGeometry": "true", "returnTrueCurves": "false", "maxAllowableOffset": "",
                      "geometryPrecision": "", "outSR": "", "havingClause": "", "returnIdsOnly": "false",
                      "returnCountOnly": "false", "orderByFields": "", "groupByFieldsForStatistics": "",
                      "outStatistics": "", "returnZ": "false", "returnM": "false", "gdbVersion": "",
                      "historicMoment": "", "returnDistinctValues": "false", "resultOffset": "",
                      "resultRecordCount": "", "returnExtentOnly": "false", "datumTransformation": "",
                      "parameterValues": "", "rangeValues": "", "quantizationParameters": "",
                      "featureEncoding": "esriDefault", "f": "geojson"}

    # Get object IDs
    objectid_list = get_object_ids(f"{service_url}/query", query_params)
    print(objectid_list)

    # Get wetland data
    nwi_gdf_final = get_wetland_data(f"{service_url}/query", query_params_2, objectid_list)

    # Convert to nad 83 coordinates
    nwi_gdf_final = nwi_gdf_final.to_crs(4269)
    print(nwi_gdf_final)
