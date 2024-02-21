import geopandas, pandas, numpy

def geol_in_shed(geol_gdf, shed_gdf):
    """
    Function to clip geologic data by the shed polygon and join the geology and watershed attributes.
    :param geol_gdf: GeoDataFrame of SGMC/geology data, should be polygon geometry.
    :param shed_gdf: GeoDataFrame of Watershed(s), should be polygon geometry.
    :return: GeoDataFrame of Geologic Units in provided watersheds, with joined attributes from both input datasets.
    """
    try:
        # Reproject both GeoDataFrames to Albers Equal Area (EPSG:5070)
        geol_gdf_albers = geol_gdf.to_crs(epsg=5070)
        # print(nwi_gdf_albers)
        shed_gdf_albers = shed_gdf.to_crs(epsg=5070)
        # print(shed_gdf_albers)
    except Exception as e:
        print("error with crs: ", e)
        return None

    try:
        # Perform intersection to retain wetlands in watersheds
        # print('trying intersection')
        geol_in_shed = geopandas.overlay(geol_gdf_albers, shed_gdf_albers, how='intersection')
        # print(nwi_in_shed)

        # # Only retain necessary columns
        # columns_to_keep = ['attribute', 'wetland_ty', 'acres', 'shape_area', 'system', 'wet_class', 'geometry']
        # nwi_in_shed = nwi_in_shed[columns_to_keep]

        return geol_in_shed
    except Exception as e:
        print(e)
        return None


def calc_area_sgmc(input_gdf):
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

camels_sheds = geopandas.read_file(
            'E:/SDSU_GEOG/Thesis/Data/CAMELS/basin_set_full_res/HCDN_nhru_final_671.shp')

camels_sheds_2 = camels_sheds.loc[:, ['hru_id', 'geometry']]
camels_sheds_2 = camels_sheds_2.rename(columns={'hru_id': 'gauge_id'})
camels_sheds_2['gauge_id'] = camels_sheds_2['gauge_id'].astype(str).str.zfill(8)

camels_test = camels_sheds_2.iloc[[0]]
# print(camels_test)

geol_test = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/01013500_sgmc_geology.shp')

clip_test = geol_in_shed(geol_test, camels_test)
print(clip_test)

area_test = calc_area_sgmc(clip_test)
print(area_test)

# Group by rock type and calculate the average age and total shape area within each group
grouped_data = area_test.groupby(['GENERALIZE', 'gauge_id']).agg({'MAX_MA': 'mean', 'area_km2': 'sum'}).reset_index()
grouped_data.rename(columns={'area_km2': 'area_km2', 'MAX_MA': 'average_age'}, inplace=True)