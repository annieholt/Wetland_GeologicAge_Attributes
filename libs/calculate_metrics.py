# Functions to generate wetland metrics for each watershed of interest
# Thus far, the metrics are related to total areas of the features

import geopandas, pandas, numpy


# Calculate the total area of each polygon feature (reproject to equals area projection and calculate)
# Can be applied to wetland and watershed shapefiles

def calc_area(input_gdf):
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



test_nwi = geopandas.read_file("C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data/nwi_wetlands.shp")

test_area = calc_area(test_nwi)
print(test_area)

# retain only wetland polygons in provided watershed polygon(s)
# this is optional in the workflow, but allows for additional NWI data reduction
# for example, if downloaded California NWI data but only needed wetlands for 8 specified California watersheds

def wetlands_in_shed(nwi_gdf, shed_gdf):
    """
    :param nwi_gdf: GeoDataFrame of NWI data, should be polygon geometry.
    :param shed_gdf: GeoDataFrame of Watershed(s), should be polygon geometry.
    :return: GeoDataFrame of NWI wetlands in provided watersheds.
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

    # Print out the unique attribute letters for checking purposes
    filter_results = nwi_gdf['system'].unique()
    print(filter_results)

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

    type_results = ['est', 'est', 'fresh', 'fresh', 'fresh', 'lake', 'other', 'other']

    # Create a new column based on conditions
    nwi_gdf['wet_class'] = numpy.select(type_conditions, type_results)
    print(nwi_gdf)

    # Print out the unique wetland class values for checking purposes
    class_results = nwi_gdf['wet_class'].unique()
    print(class_results)

    return nwi_gdf

# NWI/watershed spatial operation
# Joining NWI wetlands to watershed(s) of interest.
# Then summarizing, generating various wetland metrics (wetland area, area fraction, etc.)
def calc_wetland_metrics(nwi_gdf, shed_gdf):
    """
    Function to summarize wetland characteristics in a given catchment, based on wetland types and coverage areas.
    :param nwi_gdf: GeoDataFrame of NWI data, should be polygon geometry.
    Should have km2 area calculated as well as 'System' column (results from nwi_prep function).
    :param shed_gdf: shed_gdf: GeoDataFrame of Watershed(s), should be polygon geometry. Should also have km2 area calculated.
    :return: GeoDataFrame of wetland summary info for watersheds.
    """
    # Check if all necessary columns exist
    required_columns = {'area_km2', 'system'}
    if not required_columns.issubset(nwi_gdf.columns) or 'area_km2' not in shed_gdf.columns:
        print("Required columns are missing.")
        return None

    # Rename area_km2 of watershed to distinguish it from wetland areas
    shed_gdf.rename(columns={'area_km2': 'shed_area_km2'}, inplace=True)

    # Perform a left spatial join of NWI wetlands to the watershed(s) GeoDataFrame
    nwi_shed = geopandas.sjoin(shed_gdf, nwi_gdf, how="left")

    # Fill NaN values explicitly for missing data
    nwi_shed['system'].fillna('None', inplace=True)
    nwi_shed['wet_class'].fillna('None', inplace=True)
    nwi_shed['area_km2'].fillna(0, inplace=True)

    # Group by each watershed and wetland class and summarize the total area
    shed_sum = nwi_shed.groupby(['hru_id', 'wet_class', 'shed_area_km2'])['area_km2'].sum().reset_index()

    # Calculate the area fraction
    shed_sum['area_frac'] = shed_sum['area_km2'] / shed_sum['shed_area_km2']

    # Pivot the data for easier visualization, one row per hru_id, one column per wetland class
    shed_sum_pivot = shed_sum.pivot(index=['hru_id', 'shed_area_km2'], columns='wet_class', values='area_frac')

    # Merge the summary data back to the watershed shapefile
    shed_final = shed_gdf.merge(shed_sum_pivot, on=['hru_id', 'shed_area_km2'])

    return shed_final
