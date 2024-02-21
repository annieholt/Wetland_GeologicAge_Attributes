# Functions to generate wetland metrics for each watershed of interest
# Thus far, the metrics are related to total areas of the features

import geopandas, pandas, numpy


# Calculate the total area of each polygon feature (reproject to equals area projection and calculate)
# Can be applied to wetland and watershed shapefiles

def calc_area_polygons(input_gdf):
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
        gdf_area["shed_area"] = gdf_area['geometry'].area / 10 ** 6
        return gdf_area
    else:
        print("No Polygon Type Geometry.")
        return None


# retain only wetland or geology polygons in provided watershed polygon(s)
# also allows for additional data reduction
# for example, if downloaded California NWI data but only needed wetlands for 8 specified California watersheds
def polygons_in_shed(attrib_gdf, shed_gdf):
    """
    Function to retain only the attribute data polygons with in the provided watershed polygon and join the wetland/
    geolgy and watershed shapefile attributes.
    :param attrib_gdf: GeoDataFrame of data such as NWI or SGMC data, should be polygon geometry.
    :param shed_gdf: GeoDataFrame of Watershed(s), should be polygon geometry.
    :return: GeoDataFrame of the features in provided watersheds, with joined attributes from both input datasets.
    """
    try:
        # Reproject both GeoDataFrames to Albers Equal Area (EPSG:5070)
        attrib_gdf_albers = attrib_gdf.to_crs(epsg=5070)
        # print(attrib_gdf_albers)
        shed_gdf_albers = shed_gdf.to_crs(epsg=5070)
        # print(shed_gdf_albers)
    except Exception as e:
        print("error with crs: ", e)
        return None

    try:
        # Perform intersection to retain polygons features in watersheds
        # print('trying intersection')
        attrib_in_shed = geopandas.overlay(attrib_gdf_albers, shed_gdf_albers, how='intersection')
        # print(nwi_in_shed)

        return attrib_in_shed
    except Exception as e:
        print(e)
        return None


def prep_nwi(nwi_gdf):
    """
    Function to create wetland categories based on wetland type, which can be used to summarize wetlands in catchments.
    :param nwi_gdf: GeoDataFrame of NWI data, should be polygon geometry. columns are 'attribute', 'wetland_type', 'area_km2'
    :return: GeoDataFrame of NWI wetlands with updated attribute information.
    """
    # Make sure all column names lowercase
    nwi_gdf.columns = nwi_gdf.columns.str.lower()

    # Retain columns of interest
    nwi_gdf_subset = nwi_gdf[['attribute', 'wetland_ty', 'geometry']]
    nwi_gdf_subset = nwi_gdf_subset.rename(columns={'wetland_ty': 'wet_type'})


    # Add a new column with just the leading ATTRIBUTE letter for simplification
    nwi_gdf_subset['system'] = nwi_gdf_subset['attribute'].str[:1]
    # # Print out the unique attribute letters for checking purposes
    # filter_results = nwi_gdf['system'].unique()
    # print(filter_results)

    # Adding columns based on wetland type
    type_conditions = [
        (nwi_gdf_subset['wet_type'] == 'Estuarine and Marine Wetland'),
        (nwi_gdf_subset['wet_type'] == 'Estuarine and Marine Deepwater'),
        (nwi_gdf_subset['wet_type'] == 'Freshwater Emergent Wetland'),
        (nwi_gdf_subset['wet_type'] == 'Freshwater Forested/Shrub Wetland'),
        (nwi_gdf_subset['wet_type'] == 'Freshwater Pond'),
        (nwi_gdf_subset['wet_type'] == 'Lake'),
        (nwi_gdf_subset['wet_type'] == 'Lakes'),
        (nwi_gdf_subset['wet_type'] == 'Riverine'),
        (nwi_gdf_subset['wet_type'] == 'Other')
    ]
    # using on above type conditions, sort into more general wetland class categories
    # based on methods in Gnann et al., 2021
    type_results = ['est', 'est', 'fresh', 'fresh', 'fresh', 'lake', 'lake', 'other', 'other']

    # Create a new column based on above assignments
    nwi_gdf_subset['wet_class'] = numpy.select(type_conditions, type_results)
    # print(nwi_gdf)

    # # Print out the unique wetland class values for checking purposes
    # class_results = nwi_gdf['wet_class'].unique()
    # print(class_results)

    return nwi_gdf_subset


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
    required_columns = {'gauge_id', 'area_km2', 'system', 'wet_type', 'shed_area'}
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
    shed_sum = nwi_gdf.groupby(['gauge_id', 'wet_class', 'shed_area'])['area_km2'].sum().reset_index()

    # Calculate the area fraction
    shed_sum['area_frac'] = shed_sum['area_km2'] / shed_sum['shed_area']
    # print(shed_sum)

    # Pivot the data for easier visualization, one row per hru_id, one column per wetland class
    # shed_sum_pivot = shed_sum.pivot(index=['gauge_id'], columns='wet_class', values='area_frac')
    shed_sum_pivot = shed_sum.pivot(index=['gauge_id'], columns='wet_class', values='area_frac')
    shed_reset = shed_sum_pivot.reset_index()
    # print(shed_final)

    # Merge the summary data back to the watershed shapefile so the output is geodataset
    shed_final = shed_gdf.merge(shed_reset, on=['gauge_id'])

    return shed_final

def calc_geol_metrics(geol_gdf, shed_gdf):
    """
    Function to summarize wetland characteristics in a given catchment, based on wetland types and coverage areas.
    :param geol_gdf: GeoDataFrame of SGMC geology data (linked with age data), should be polygon geometry.
    Should have km2 area calculated.
    :param shed_gdf: GeoDataFrame of watershed, should be a polygon geometry. Includes 'gauge_id' column.
    :return: GeoDataFrame of wetland summary info for watersheds.
    """
    # take the average of the min and max ages, in millions of years, for each polygon
    geol_gdf['AV_MA'] = (geol_gdf['MIN_MA'] + geol_gdf['MAX_MA']) / 2

    # Group by rock type and calculate the average age and total shape area within each group
    geol_type_sum = geol_gdf.groupby(['gauge_id', 'GENERALIZE', 'shed_area']).agg(
        {'AV_MA': 'mean', 'area_km2': 'sum'}).reset_index()
    geol_type_sum.rename(columns={'area_km2': 'area_km2', 'AV_MA': 'av_age'}, inplace=True)

    # Calculate the area fraction
    geol_type_sum['area_frac'] = geol_type_sum['area_km2'] / geol_type_sum['shed_area']

    # calculate the area weighted age for each unit
    geol_type_sum['av_age_w'] = geol_type_sum['area_frac'] * geol_type_sum['av_age']

    # calculate the area-weighted average age of the catchment
    # geol_age = geol_type_sum['av_age_w'].sum()
    geol_age = geol_type_sum.groupby(['gauge_id']).agg({'av_age_w': 'sum'}).reset_index()
    print(geol_age)

    # # only retain the major geologic unit type
    # geol_major = geol_type_sum[geol_type_sum['area_frac'] == geol_type_sum['area_frac'].max()]
    #
    # geol_final = geol_major[['gauge_id', 'GENERALIZE', 'area_frac', 'av_age']].rename(columns=
    #                                                                                  {'GENERALIZE': 'major_lith',
    #                                                                                   'area_frac': 'lith_area_frac'})
    # print(geol_final)

    # pivot the data wide???
    # shed_sum_pivot = geol_final.pivot(index=['gauge_id'], columns='GENERALIZE', values='area_frac')
    # shed_reset = shed_sum_pivot.reset_index()

    # Merge the summary data back to the watershed shapefile so the output is geodataset
    shed_final = shed_gdf.merge(geol_age, on=['gauge_id'])
    # shed_final = shed_gdf.merge(geol_final, on=['gauge_id'])

    # print(shed_final)

    return shed_final


# camels_sheds = geopandas.read_file(
#             'C:/Users/aholt8450/Documents/Data/basin_set_full_res/HCDN_nhru_final_671.shp')
# camels_sheds = geopandas.read_file(
#             'E:/SDSU_GEOG/Thesis/Data/CAMELS/basin_set_full_res/HCDN_nhru_final_671.shp')
#
# camels_sheds_2 = camels_sheds.loc[:, ['hru_id', 'geometry']]
# camels_sheds_2 = camels_sheds_2.rename(columns={'hru_id': 'gauge_id'})
# camels_sheds_2['gauge_id'] = camels_sheds_2['gauge_id'].astype(str).str.zfill(8)
#
# camels_test = camels_sheds_2.iloc[[0]]
# # print(camels_test)
#
# shed_area = calc_area_shed(camels_test)
# print(shed_area)
# geol_test = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/01013500_sgmc_geology.shp')
# clip_test = polygons_in_shed(geol_test, shed_area)
# print(clip_test)
# area_test = calc_area_polygons(clip_test)
# print(area_test)
# geol_metrics_test = calc_geol_metrics(area_test, shed_area)
# print(geol_metrics_test)






# # nwi_test = geopandas.read_file('C:/Users/aholt8450/Documents/Data/NWI_camels/01013500_nwi_wetlands.shp')
#
# nwi_test = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/NWI_camels/01013500_nwi_wetlands.shp')
#
# # shed area first
# shed_area = calc_area_shed(camels_test)
#
# nwi_prep = prep_nwi(nwi_test)
# # print(nwi_prep)
#
# # then making sure to only have wetlands within watershed boundary (not just intersecting)
# nwi_shed_join = wetlands_in_shed(nwi_prep, shed_area)
# print(nwi_shed_join)
#
# # now calculating wetland areas
# nwi_area = calc_area_nwi(nwi_shed_join)
# print(nwi_area)
#
# nwi_metrics = calc_wetland_metrics(nwi_area, shed_area)
# print(nwi_metrics)




# # prep nwi data and run metrics calculation for each camels watershed
# import os
# ref_sheds = geopandas.read_file(
#     'C:/Users/aholt8450/Documents/Data/Gages-II/boundaries-shapefiles-by-aggeco/bas_ref_all_conus.shp')
#
# ref_sheds_2 = ref_sheds.loc[:, ['GAGE_ID', 'geometry']]
# ref_sheds_2 = ref_sheds_2.rename(columns={'GAGE_ID': 'gauge_id'})
# # ref_sheds_2['gauge_id'] = ref_sheds_2['gauge_id'].astype(str).str.zfill(8)
#
# flow_files = os.listdir('C:/Users/aholt8450/Documents/Data/Gages-II/usgs_streamflow_2/mm_day')
# ids_list = []
#
# for name in flow_files:
#     gauge_id = name.split('.csv')[0]
#     ids_list.append(gauge_id)
# # print(ids_list)
#
# # only get NWI data for the new reference watersheds (not including those in camels or failed downloads)
# ref_sheds_filtered = ref_sheds_2[ref_sheds_2['gauge_id'].isin(ids_list)]
# # print(ref_sheds_filtered)
# shed_gdf = ref_sheds_filtered.loc[ref_sheds_filtered['gauge_id'] == '01029200']
# # print(shed_gdf)
#
# nwi_path = 'C:/Users/aholt8450/Documents/Data/NWI_gagesII'
# gauge_id = shed_gdf['gauge_id'].iloc[0]
# file_name = gauge_id + '_nwi_wetlands.shp'
# file_path = os.path.join(nwi_path, file_name)
# print(file_path)
# nwi_gdf = geopandas.read_file(file_path)
# print(nwi_gdf)
# # metrics processing; below function order is required
# shed_area = calc_area_shed(shed_gdf)
# nwi_prep = prep_nwi(nwi_gdf)
# print(nwi_prep)
# nwi_shed_join = wetlands_in_shed(nwi_prep, shed_area)
# print(nwi_shed_join)
# nwi_area = calc_area_nwi(nwi_shed_join)
# nwi_metrics = calc_wetland_metrics(nwi_area, shed_area)
# print(nwi_metrics)