# Functions to generate wetland metrics for each watershed of interest
# Thus far, the metrics are related to total areas of the features

import geopandas, pandas, numpy


# Calculate the total area of each polygon feature (reproject to equals area projection and calculate)
# Can be applied to wetland and watershed shapefiles

def calc_area_nwi(input_gdf):
    """
    :param input_gdf: Input GeoDataFrame. Should be polygon geometry type.
    :return: a GeoDataFrame.
    """
    try:
        # Reproject to Albers Equal Area Projection (EPSG:5070)
        gdf_alb = input_gdf.to_crs(epsg=5070)
    except Exception as e:
        print(e)
        return None

    # Filter for valid polygon and multipolygon geometries
    valid_types = ['Polygon', 'MultiPolygon']
    gdf_area = gdf_alb[gdf_alb['geometry'].geom_type.isin(valid_types)]

    if not gdf_area.empty:
        # Calculate area in square kilometers
        gdf_area["area_km2"] = gdf_area['geometry'].area / 10 ** 6
        return gdf_area
    else:
        print("No Polygon Type Geometry.")
        return None


def calc_area_shed(input_gdf):
    """
    :param input_gdf: Input GeoDataFrame. Should be polygon geometry type.
    :return: a GeoDataFrame.
    """
    try:
        # Reproject to Albers Equal Area Projection (EPSG:5070)
        gdf_alb = input_gdf.to_crs(epsg=5070)
    except Exception as e:
        print(e)
        return None

    # Filter for valid polygon and multipolygon geometries
    valid_types = ['Polygon', 'MultiPolygon']
    gdf_area = gdf_alb[gdf_alb['geometry'].geom_type.isin(valid_types)]

    if not gdf_area.empty:
        # Calculate area in square kilometers
        gdf_area["shed_area_km2"] = gdf_area['geometry'].area / 10 ** 6
        return gdf_area
    else:
        print("No Polygon Type Geometry.")
        return None


# retain only wetland polygons in provided watershed polygon(s)
# this is optional in the workflow, but allows for additional NWI data reduction
# for example, if downloaded California NWI data but only needed wetlands for 8 specified California watersheds
def wetlands_in_shed(nwi_gdf, shed_gdf):
    """
    Function to retain only the wetland polygons with in the provided watershed polygon and join the wetland
    and watershed shapefile attributes.
    :param nwi_gdf: GeoDataFrame of NWI data, should be polygon geometry.
    :param shed_gdf: GeoDataFrame of Watershed(s), should be polygon geometry.
    :return: GeoDataFrame of NWI wetlands in provided watersheds, with joined attributes from both input datasets.
    """
    try:
        # Reproject both GeoDataFrames to Albers Equal Area (EPSG:5070)
        nwi_gdf_albers = nwi_gdf.to_crs(epsg=5070)
        shed_gdf_albers = shed_gdf.to_crs(epsg=5070)
    except Exception as e:
        print(e)
        return None

    try:
        # Perform intersection to retain wetlands in watersheds
        nwi_in_shed = geopandas.overlay(nwi_gdf_albers, shed_gdf_albers, how='intersection')

        # # Only retain necessary columns
        # columns_to_keep = ['attribute', 'wetland_ty', 'acres', 'shape_area', 'system', 'wet_class', 'geometry']
        # nwi_in_shed = nwi_in_shed[columns_to_keep]

        return nwi_in_shed
    except Exception as e:
        print(e)
        return None


def prep_nwi(nwi_gdf):
    """
    Function to create wetland categories based on wetland type, which can be used to summarize wetlands in catchments.
    :param nwi_gdf: GeoDataFrame of NWI data, should be polygon geometry. columns are 'attribute', 'wet_type', 'area_km2'
    :return: GeoDataFrame of NWI wetlands with updated attribute information.
    """
    # Make sure all column names lowercase
    nwi_gdf.columns = nwi_gdf.columns.str.lower()

    # Retain columns of interest
    # nwi_filtered = nwi_gdf[['attribute', 'wetland_ty', 'area_km2', 'geometry']].copy()

    # Add a new column with just the leading ATTRIBUTE letter for simplification
    nwi_gdf['system'] = nwi_gdf['attribute'].str[:1]
    # # Print out the unique attribute letters for checking purposes
    # filter_results = nwi_gdf['system'].unique()
    # print(filter_results)

    # Adding columns based on wetland type
    type_conditions = [
        (nwi_gdf['wet_type'] == 'Estuarine and Marine Wetland'),
        (nwi_gdf['wet_type'] == 'Estuarine and Marine Deepwater'),
        (nwi_gdf['wet_type'] == 'Freshwater Emergent Wetland'),
        (nwi_gdf['wet_type'] == 'Freshwater Forested/Shrub Wetland'),
        (nwi_gdf['wet_type'] == 'Freshwater Pond'),
        (nwi_gdf['wet_type'] == 'Lake'),
        (nwi_gdf['wet_type'] == 'Riverine'),
        (nwi_gdf['wet_type'] == 'Other')
    ]
    # using on above type conditions, sort into more general wetland class categories
    # based on methods in Gnann et al., 2021
    type_results = ['est', 'est', 'fresh', 'fresh', 'fresh', 'lake', 'other', 'other']

    # Create a new column based on above assignments
    nwi_gdf['wet_class'] = numpy.select(type_conditions, type_results)
    # print(nwi_gdf)

    # # Print out the unique wetland class values for checking purposes
    # class_results = nwi_gdf['wet_class'].unique()
    # print(class_results)

    return nwi_gdf


# NWI/watershed spatial operation
# Joining NWI wetlands to watershed(s) of interest.
# Then summarizing, generating various wetland metrics (wetland area, area fraction, etc.)
def calc_wetland_metrics(nwi_gdf, shed_gdf):
    """
    Function to summarize wetland characteristics in a given catchment, based on wetland types and coverage areas.
    :param nwi_gdf: GeoDataFrame of NWI data, should be polygon geometry.
    Should have km2 area calculated as well as 'System' column (results from nwi_prep function).
    :param shed_gdf: GeoDataFrame of watershed, should be a polygon geometry. Includes 'gauge_id' column.
    :return: GeoDataFrame of wetland summary info for watersheds.
    """
    # Check if all necessary columns exist
    required_columns = {'gauge_id', 'area_km2', 'system', 'wet_type', 'shed_area_km2'}
    if not required_columns.issubset(nwi_gdf.columns):
        print("Required columns are missing.")
        return None

    # if not required_columns.issubset(nwi_gdf.columns) or 'area_km2' not in shed_gdf.columns:
    #     print("Required columns are missing.")
    #     return None
    #
    # # Rename area_km2 of watershed to distinguish it from wetland areas
    # shed_gdf.rename(columns={'area_km2': 'shed_area_km2'}, inplace=True)
    #
    # # Perform a left spatial join of NWI wetlands to the watershed(s) GeoDataFrame
    # nwi_shed = geopandas.sjoin(shed_gdf, nwi_gdf, how="left")

    # Fill NaN values explicitly for missing data (may remove this step??)
    nwi_gdf['system'].fillna('None', inplace=True)
    nwi_gdf['wet_class'].fillna('None', inplace=True)
    nwi_gdf['area_km2'].fillna(0, inplace=True)

    # Group by each watershed and wetland class and summarize the total area
    shed_sum = nwi_gdf.groupby(['gauge_id', 'wet_class', 'shed_area_km2'])['area_km2'].sum().reset_index()

    # Calculate the area fraction
    shed_sum['area_frac'] = shed_sum['area_km2'] / shed_sum['shed_area_km2']
    # print(shed_sum)

    # Pivot the data for easier visualization, one row per hru_id, one column per wetland class
    # shed_sum_pivot = shed_sum.pivot(index=['gauge_id'], columns='wet_class', values='area_frac')
    shed_sum_pivot = shed_sum.pivot(index=['gauge_id'], columns='wet_class', values='area_frac')
    shed_final = shed_sum_pivot.reset_index()
    # print(shed_final)

    # Merge the summary data back to the watershed shapefile so the output is geodataset
    shed_final = shed_gdf.merge(shed_sum_pivot, on=['gauge_id'])

    return shed_final


import fiona
from shapely.geometry import shape, Polygon
import geopandas

# Replace 'your_geodatabase.gdb' with the path to your geodatabase.
# geodatabase_path = "C:/Users/aholt8450/Documents/Data/NWI_testing.gdb"
geodatabase_path = "E:/SDSU_GEOG/Thesis/Data/NWI_CONUS/NWI_testing.gdb"

# Replace 'your_layer' with the name of the layer you want to query.
layer_name = 'Wetlands_Merge_CONUS'

# Replace 'your_polygon_layer.shp' with the path to your polygon layer.
# polygon_layer_path = 'C:/Users/aholt8450/Documents/Data/camels_test_basin.shp'
polygon_layer_path = 'C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/camels_test_basin.shp'

# names = fiona.listlayers("E:/SDSU_GEOG/Thesis/Data/NWI_CONUS/NWI_testing.gdb")
# print(names)

polygon_gdf = geopandas.read_file(polygon_layer_path)
polygon_gdf_2 = polygon_gdf.to_crs(epsg=5070)
bbox = polygon_gdf_2.total_bounds.tolist()
print(bbox)

# out_gdf = geopandas.read_file(geodatabase_path, driver='FileGDB', layer=layer_name, bbox=bbox)

out_gdf = geopandas.read_file(geodatabase_path, driver='FileGDB', layer=layer_name, mask=polygon_gdf_2)

out_gdf.to_file('C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/please_work_2.shp')











# # Load the polygon shapefile
# bounding_box_gdf = geopandas.read_file(polygon_layer_path)
#
# # Calculate the bounding box of the shapefile
# bounding_box = bounding_box_gdf.total_bounds
# minx, miny, maxx, maxy = bounding_box
#
# # Create a bounding box as a Shapely geometry
# bbox = box(minx, miny, maxx, maxy)
#
# # Open the geodatabase file
# gdb = geopandas.GeoDataFrame()
#
# with fiona.open(geodatabase_path, layer=layer_name) as src:
#     for feature in src:
#         feature_shape = shape(feature['geometry'])
#         if feature_shape.intersects(bbox):
#             gdb = gdb.append(geopandas.GeoDataFrame([feature]))
#
# # Do something with the query result, e.g., print it
# print(gdb)
#
# # # You can also save the result to a new shapefile if needed
# # gdb.to_file("query_result.shp")
