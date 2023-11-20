# Workflow for CAMELS and GAGES II Catchments

# download NWI data for catchments and generate wetland metrics

# download usgs streamflow data for gages II watersheds

# importing module libraries
# from libs.dataretrieval_api import *
import multiprocessing
import re

from libs.calculate_metrics import *
from libs.dataretrieval_nwis import *
import os


# def nwi_iter(shed_gdf):
#     try:
#         # data retrieval
#         # nwi_gdf = nwi_download_api(shed_gdf=shed_gdf, out_dir="E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles",
#         #                            save=False)
#         # # metrics processing
#         # nwi_area = calc_area_nwi(nwi_gdf)
#         # shed_area = calc_area_shed(shed_gdf)
#         # nwi_shed_join = wetlands_in_shed(nwi_area, shed_area)
#         # nwi_prep = prep_nwi(nwi_shed_join)
#         # nwi_metrics = calc_wetland_metrics(nwi_prep, shed_area)
#
#     except Exception as e:
#         print(f"An error occurred: {e}")
#         return None
#
#     return out_gdf

def nwi_camels_download():
    # iterate data retrieval for each camels watershed
    # camels_sheds = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/CAMELS/basin_set_full_res/HCDN_nhru_final_671.shp')
    camels_sheds = geopandas.read_file(
        'C:/Users/aholt8450/Documents/Data/basin_set_full_res/HCDN_nhru_final_671.shp')

    camels_sheds_2 = camels_sheds.loc[:, ['hru_id', 'geometry']]
    camels_sheds_2 = camels_sheds_2.rename(columns={'hru_id': 'gauge_id'})
    camels_sheds_2['gauge_id'] = camels_sheds_2['gauge_id'].astype(str).str.zfill(8)

    # first, testing a smaller subset
    # camels_sheds_test = camels_sheds_2.head(10).copy()
    # print(camels_sheds_test)

    # empty list for watershed data
    camels_sheds_list = []

    # Loop through each row in the original GeoDataFrame
    for index, row in camels_sheds_2.iterrows():
        try:
            # Create a new GeoDataFrame with a single row
            single_row_gdf = camels_sheds_2.iloc[[index]]
            # print(single_row_gdf)

            # Append it to the list
            camels_sheds_list.append(single_row_gdf)

            # import nwi data from geodatabase
            # note that this is equivalent to intersection rather than clip, so sometimes the features extend
            geodatabase_path = "C:/Users/aholt8450/Documents/ArcGIS/Projects/NWI_testing/NWI_testing.gdb"
            layer_name = 'Wetlands_Merge_CONUS'
            out_dir = 'C:/Users/aholt8450/Documents/Data/NWI_camels'

            out_gdf = geopandas.read_file(geodatabase_path, driver='FileGDB', layer=layer_name, mask=single_row_gdf)
            # print(out_gdf)
            out_gdf = out_gdf.reset_index(drop=True)
            # print(out_gdf)

            # getting watershed id
            gauge_id = single_row_gdf['gauge_id'].iloc[0]
            print(gauge_id)
            # output path for data
            file_name = gauge_id + '_nwi_wetlands.shp'
            # print(file_name)
            # create pull file path
            file_path = os.path.join(out_dir, file_name)
            print(file_path)
            # save the GeoDataFrame as a shapefile
            out_gdf.to_file(file_path, index=False)
            print(f"Downloaded data and saved as {file_path}")

        except Exception as e:
            print(f"An error occurred: {e}")


def nwi_metrics_workflow_camels():
    # prep nwi data and run metrics calculation for each camels watershed
    camels_sheds = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/CAMELS/basin_set_full_res/HCDN_nhru_final_671.shp')

    # # first, testing a smaller subset
    # camels_sheds_test = camels_sheds.head(10).copy()
    # # print(camels_sheds_test)

    # Initialize an empty list to store the results
    results = []

    for index, row in camels_sheds.iterrows():

        try:
            # create a geodataframe for current watershed
            single_row_gdf = camels_sheds.iloc[[index]]

            # removing unneeded attribute data and making sure ID column has 8 values (leading zero sometimes)
            shed_gdf = single_row_gdf.loc[:, ['hru_id', 'geometry']]
            shed_gdf = shed_gdf.rename(columns={'hru_id': 'gauge_id'})
            shed_gdf['gauge_id'] = shed_gdf['gauge_id'].astype(str).str.zfill(8)
            # print(shed_gdf)

            # # data retrieval
            # nwi_gdf = nwi_download_api(shed_gdf=shed_gdf, out_dir="E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles",
            #                            save=True)

            nwi_path = 'E:/SDSU_GEOG/Thesis/Data/NWI_camels'
            gauge_id = shed_gdf['gauge_id'].iloc[0]
            file_name = gauge_id + '_nwi_wetlands.shp'
            # print(file_name)
            # create pull file path
            file_path = os.path.join(nwi_path, file_name)
            print(file_path)
            nwi_gdf = geopandas.read_file(file_path)
            # print(nwi_gdf)

            # metrics processing; below function order is required
            shed_area = calc_area_shed(shed_gdf)
            nwi_prep = prep_nwi(nwi_gdf)
            nwi_shed_join = wetlands_in_shed(nwi_prep, shed_area)
            nwi_area = calc_area_nwi(nwi_shed_join)
            nwi_metrics = calc_wetland_metrics(nwi_area, shed_area)

            # export just in case for now
            file_name_export = gauge_id + '_wetland_metrics.shp'
            # create pull file path
            file_path_export = os.path.join('E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/by_id',
                                            file_name_export)
            print(file_path_export)
            nwi_metrics.to_file(file_path_export, index=False)

            # add to results
            results.append(nwi_metrics)

        except Exception as e:
            print(f"An error occurred: {e}")

    result_gdf = pandas.concat(results, ignore_index=True)
    # print(result_gdf)
    result_gdf.to_file("E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/nwi_camels_metrics.shp")


def nwi_gagesII_download():
    # iterate data retrieval for each reference gages II watershed
    ref_sheds = geopandas.read_file(
        'C:/Users/aholt8450/Documents/Data/Gages-II/boundaries-shapefiles-by-aggeco/bas_ref_all.shp')

    ref_sheds_2 = ref_sheds.loc[:, ['GAGE_ID', 'geometry']]
    ref_sheds_2 = ref_sheds_2.rename(columns={'GAGE_ID': 'gauge_id'})
    # ref_sheds_2['gauge_id'] = ref_sheds_2['gauge_id'].astype(str).str.zfill(8)
    # print(ref_sheds_2)

    # want to just do the reference watersheds we also have flow data for, for now
    # so reading in names of flow data files, and using the gage IDs to filter the sheds

    flow_files = os.listdir('C:/Users/aholt8450/Documents/Data/Gages-II/usgs_streamflow_2/mm_day')
    # print(flow_files)
    ids_list = []

    for name in flow_files:
        gauge_id = name.split('.csv')[0]
        ids_list.append(gauge_id)

    # print(ids_list)

    # only get NWI data for the new reference watersheds (not including those in camels or failed downloads)
    ref_sheds_filtered = ref_sheds_2[ref_sheds_2['gauge_id'].isin(ids_list)]
    # print(ref_sheds_filtered)

    # ref_sheds_filtered_2 = ref_sheds_filtered[]

    # empty list for watershed data
    ref_sheds_list = []

    # Loop through each row in the original GeoDataFrame
    for row in ref_sheds_filtered.itertuples(index=True, name='RowData'):
        try:
            # Create a new GeoDataFrame with a single row
            single_row_gdf = geopandas.GeoDataFrame([row], geometry='geometry', crs=ref_sheds_filtered.crs)
            # single_row_gdf = ref_sheds_filtered.iloc[[index]]
            # print(single_row_gdf)

            # Append it to the list
            ref_sheds_list.append(single_row_gdf)

            # import nwi data from geodatabase
            # note that this is equivalent to intersection rather than clip, so sometimes the features extend
            geodatabase_path = "C:/Users/aholt8450/Documents/ArcGIS/Projects/NWI_testing/NWI_testing.gdb"
            layer_name = 'Wetlands_Merge_CONUS'
            out_dir = 'C:/Users/aholt8450/Documents/Data/NWI_gagesII'

            out_gdf = geopandas.read_file(geodatabase_path, driver='FileGDB', layer=layer_name, mask=single_row_gdf)
            # print(out_gdf)
            out_gdf = out_gdf.reset_index(drop=True)
            # print(out_gdf)

            # getting watershed id
            gauge_id = single_row_gdf['gauge_id'].iloc[0]
            print(gauge_id)
            # output path for data
            file_name = gauge_id + '_nwi_wetlands.shp'
            # print(file_name)
            # create pull file path
            file_path = os.path.join(out_dir, file_name)
            print(file_path)
            # save the GeoDataFrame as a shapefile
            out_gdf.to_file(file_path, index=False)
            print(f"Downloaded data and saved as {file_path}")

        except Exception as e:
            print(f"An error occurred: {e}")


def nwi_metrics_workflow_gagesII():
    # prep nwi data and run metrics calculation for each camels watershed
    ref_sheds = geopandas.read_file(
        'C:/Users/aholt8450/Documents/Data/Gages-II/boundaries-shapefiles-by-aggeco/bas_ref_all_conus.shp')

    # ref_sheds = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/Gages-II/boundaries-shapefiles-by-aggeco/bas_ref_all.shp')

    ref_sheds_2 = ref_sheds.loc[:, ['GAGE_ID', 'geometry']]
    ref_sheds_2 = ref_sheds_2.rename(columns={'GAGE_ID': 'gauge_id'})
    # ref_sheds_2['gauge_id'] = ref_sheds_2['gauge_id'].astype(str).str.zfill(8)

    flow_files = os.listdir('C:/Users/aholt8450/Documents/Data/Gages-II/usgs_streamflow_2/mm_day')
    # flow_files = os.listdir('E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow_2/mm_day')
    # print(flow_files)
    ids_list = []

    for name in flow_files:
        gauge_id = name.split('.csv')[0]
        ids_list.append(gauge_id)

    # print(ids_list)

    # only get NWI data for the new reference watersheds (not including those in camels or failed downloads)
    ref_sheds_filtered = ref_sheds_2[ref_sheds_2['gauge_id'].isin(ids_list)]
    print(ref_sheds_filtered)

    # Initialize an empty list to store the results
    results = []

    # Loop through each row in the original GeoDataFrame
    for row in ref_sheds_filtered.itertuples(index=True, name='RowData'):
        try:
            # Create a new GeoDataFrame with a single row
            single_row_gdf = geopandas.GeoDataFrame([row], geometry='geometry', crs=ref_sheds_filtered.crs)
            # single_row_gdf = ref_sheds_filtered.iloc[[index]]

            shed_gdf = single_row_gdf
            # print(shed_gdf)

            # nwi_path = 'E:/SDSU_GEOG/Thesis/Data/NWI_gagesII'
            nwi_path = 'C:/Users/aholt8450/Documents/Data/NWI_gagesII'
            gauge_id = shed_gdf['gauge_id'].iloc[0]
            file_name = gauge_id + '_nwi_wetlands.shp'
            # print(file_name)
            # create pull file path
            file_path = os.path.join(nwi_path, file_name)
            print(file_path)
            nwi_gdf = geopandas.read_file(file_path)
            # print(nwi_gdf)

            # metrics processing; below function order is required
            shed_area = calc_area_shed(shed_gdf)
            nwi_prep = prep_nwi(nwi_gdf)
            nwi_shed_join = wetlands_in_shed(nwi_prep, shed_area)
            nwi_area = calc_area_nwi(nwi_shed_join)
            nwi_metrics = calc_wetland_metrics(nwi_area, shed_area)

            # export just in case for now
            file_name_export = gauge_id + '_wetland_metrics.shp'
            # create pull file path
            # file_path_export = os.path.join('E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/by_id_gagesII',
            #                                 file_name_export)
            file_path_export = os.path.join('C:/Users/aholt8450/Documents/Data/NWI_outputs/by_id_gagesII',
                                            file_name_export)
            print(file_path_export)
            nwi_metrics.to_file(file_path_export, index=False)

            # add to results
            results.append(nwi_metrics)

        except Exception as e:
            print(f"An error occurred: {e}")

    result_gdf = pandas.concat(results, ignore_index=True)
    # print(result_gdf)
    # result_gdf.to_file("E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/nwi_gagesII_ref_metrics.shp")
    result_gdf.to_file("C:/Users/aholt8450/Documents/Data/NWI_outputs/nwi_gagesII_ref_metrics_new.shp")


def giws_download():
    ref_sheds = geopandas.read_file(
        'E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/Catchments/camels_gagesII_final_catchments.shp')

    # ref_sheds_2 = ref_sheds.loc[:, ['gauge_id', 'geometry']]

    # print(ref_sheds)

    ref_sheds_list = []

    # Loop through each row in the original GeoDataFrame
    for row in ref_sheds.itertuples(index=True, name='RowData'):
        try:
            # Create a new GeoDataFrame with a single row
            single_row_gdf = geopandas.GeoDataFrame([row], geometry='geometry', crs=ref_sheds.crs)
            # print(single_row_gdf)

            # Append it to the list
            ref_sheds_list.append(single_row_gdf)

            # import nwi data from geodatabase
            # note that this is equivalent to intersection rather than clip, so sometimes the features extend
            geodatabase_path = "E:/SDSU_GEOG/Thesis/Data/GIWs/GIWs_CONUS.gdb"
            layer_name = 'GIWs_CONUS_final'
            out_dir = 'E:/SDSU_GEOG/Thesis/Data/GIWs/GIWs_camels_gagesII'

            out_gdf = geopandas.read_file(geodatabase_path, driver='FileGDB', layer=layer_name, mask=single_row_gdf)
            # print(out_gdf)
            out_gdf = out_gdf.reset_index(drop=True)
            # print(out_gdf)

            # getting watershed id
            gauge_id = single_row_gdf['gauge_id'].iloc[0]
            print(gauge_id)
            # output path for data
            file_name = gauge_id + '_giws.shp'
            # print(file_name)
            # create pull file path
            file_path = os.path.join(out_dir, file_name)
            print(file_path)
            # save the GeoDataFrame as a shapefile
            out_gdf.to_file(file_path, index=False)
            print(f"Downloaded data and saved as {file_path}")

        except Exception as e:
            print(f"An error occurred: {e}")



def giws_metrics_workflow():
    # prep nwi data and run metrics calculation for each camels watershed
    ref_sheds = geopandas.read_file(
        'E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/Catchments/camels_gagesII_final_catchments.shp')

    # ref_sheds = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/Gages-II/boundaries-shapefiles-by-aggeco/bas_ref_all.shp')

    ref_sheds_2 = ref_sheds.loc[:, ['gauge_id', 'geometry']]

    # Initialize an empty list to store the results
    results = []

    # Loop through each row in the original GeoDataFrame
    for row in ref_sheds_2.itertuples(index=True, name='RowData'):
        try:
            # Create a new GeoDataFrame with a single row
            single_row_gdf = geopandas.GeoDataFrame([row], geometry='geometry', crs=ref_sheds_2.crs)
            # single_row_gdf = ref_sheds_filtered.iloc[[index]]

            shed_gdf = single_row_gdf
            # print(shed_gdf)

            giw_path = 'E:/SDSU_GEOG/Thesis/Data/GIWs/GIWs_camels_gagesII'
            gauge_id = shed_gdf['gauge_id'].iloc[0]
            file_name = gauge_id + '_giws.shp'
            file_path = os.path.join(giw_path, file_name)
            print(file_path)
            giw_gdf = geopandas.read_file(file_path)
            giw_gdf_2 = giw_gdf.loc[:, ['geometry']]
            # print(giw_gdf_2)

            # metrics processing; below function order is required
            shed_area = calc_area_shed(shed_gdf)
            # print(shed_area)
            giw_shed_join = wetlands_in_shed(giw_gdf_2, shed_area)
            giw_area = calc_area_nwi(giw_shed_join)

            # Group by each watershed and wetland class and summarize the total area
            shed_sum = giw_area.groupby(['gauge_id', 'shed_area'])['area_km2'].sum().reset_index()

            # Calculate the area fraction
            shed_sum['area_frac'] = shed_sum['area_km2'] / shed_sum['shed_area']
            # print(shed_sum)

            # Merge the summary data back to the watershed shapefile so the output is geodataset
            shed_merge = shed_gdf.merge(shed_sum, on=['gauge_id'])
            shed_final = shed_merge.loc[:, ['gauge_id', 'shed_area', 'area_km2', 'area_frac', 'geometry']]
            # print(shed_final)

            # export just in case for now
            file_name_export = gauge_id + '_giw_metrics.shp'
            file_path_export = os.path.join('E:/SDSU_GEOG/Thesis/Data/GIWs/GWIs_outputs',
                                            file_name_export)
            print(file_path_export)
            shed_final.to_file(file_path_export, index=False)

            # add to results
            results.append(shed_final)

        except Exception as e:
            print(f"An error occurred: {e}")

    result_gdf = pandas.concat(results, ignore_index=True)
    # print(result_gdf)
    result_gdf.to_file("E:/SDSU_GEOG/Thesis/Data/GWIs/giws_metrics.shp")
    # result_gdf.to_file("C:/Users/aholt8450/Documents/Data/NWI_outputs/nwi_gagesII_ref_metrics_new.shp")




def download_flow():
    # using CAMELS watersheds
    camels_ids = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/CAMELS/camels_name.txt', delimiter=';')
    # convert column with ids to string and add leading zeros if they were lost
    camels_ids['gauge_id'] = camels_ids['gauge_id'].astype(str).str.zfill(8)
    # print(camels_ids)

    # now want all gaugeids in GAges II reference dataset, except CAMELs
    # # downloaded basin ID file here: https://www.sciencebase.gov/catalog/item/59692a64e4b0d1f9f05fbd39
    # gages_II_ids = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/Gages-II/BasinID.txt', delimiter=',')
    # # print(gages_II_ids)
    # gages_II_ids['STAID'] = gages_II_ids['STAID'].astype(str).str.zfill(8)
    # gages_II_ids_ref = gages_II_ids.loc[gages_II_ids['CLASS'] == 'Ref']
    # # print(len(gages_II_ids_ref['STAID']))

    gages_II_ids_ref = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/Gages-II/basinID_ref.csv', delimiter=',')
    gages_II_ids_ref['GAGE_ID'] = gages_II_ids_ref['GAGE_ID'].astype(str).str.zfill(8)
    # print(gages_II_ids_ref)

    # now just get siteids not in camels list, for reference gages
    siteids_list = list(set(gages_II_ids_ref['GAGE_ID']) - set(camels_ids['gauge_id']))
    print(siteids_list)
    print(len(siteids_list))

    # note that I changed the folder location to usgs_streamflow_2 for the second iteration
    # the first iteration I used the BasinID text file, the second I pulled gage ids from the ref shapefile instead
    # I was suspicious that the two lists of IDs were different from each other

    # downloading flow data and saving in format required for TOSSH toolbox processing
    for num in range(len(siteids_list)):
        try:
            siteid = siteids_list[num]
            print(siteid)
            out_dir_1 = "E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow_2"
            # out_dir_1 = "C:/Users/aholt8450/Documents/Data/usgs_streamflow"
            out_dir_2 = "E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow_2/mm_day"
            # out_dir_2 = "C:/Users/aholt8450/Documents/Data/usgs_streamflow/mm_day"

            drain_area = usgs_drain_area_download_api(siteid=siteid)
            print(drain_area)
            flow_cfs_df = usgs_daily_download_api(siteid=siteid, out_dir=out_dir_1, save=True)
            flow_mm_day_df = usgs_daily_prep(siteid=siteid, drain_area=drain_area,
                                             flow_cfs_df=flow_cfs_df, out_dir=out_dir_2, save=True)
        except Exception as e:
            print(f"An error occurred: {e}")


def main():
    # nwi_metrics_workflow_camels()

    # nwi_gagesII_download()

    # nwi_metrics_workflow_gagesII()

    # giws_download()

    giws_metrics_workflow()



    # import glob
    # path = 'C:/Users/aholt8450/Documents/Data/NWI_camels'
    # shp_files = glob.glob(f"{path}/*.shp")
    # num_files = len(shp_files)
    # print(num_files)

    # SIGNATURE WORKFLOW
    # download_flow()


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
