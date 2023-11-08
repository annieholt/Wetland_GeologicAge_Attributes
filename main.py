# Workflow for CAMELS and GAGES II Catchments
import libs.dataretrieval_api
# first downloading and prepping flow data
# then running MatLab TOSSH toolbox functions to generated groundwater signatures
# next, download NWI data for catchments and generate wetland metrics
# finally, analyze the relationships

# importing module libraries
from libs.dataretrieval_api import *
from libs.calculate_metrics import *
import multiprocessing


def nwi_iter(shed_gdf):
    try:
        # data retrieval
        nwi_gdf = nwi_download_api(shed_gdf=shed_gdf, out_dir="E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles",
                                   save=False)
        # metrics processing
        nwi_area = calc_area_nwi(nwi_gdf)
        shed_area = calc_area_shed(shed_gdf)
        nwi_shed_join = wetlands_in_shed(nwi_area, shed_area)
        nwi_prep = prep_nwi(nwi_shed_join)
        nwi_metrics = calc_wetland_metrics(nwi_prep, shed_area)
    except Exception as e:
        print(f"An error occurred: {e}")
        return None

    return nwi_metrics

def main():
    # WETLAND WORKFLOW, camels watersheds
    # iterate data retrieval and metrics calculation for each camels watershed

    camels_sheds = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/CAMELS/basin_set_full_res/HCDN_nhru_final_671.shp')

    camels_sheds_2 = camels_sheds.loc[:, ['hru_id', 'geometry']]
    camels_sheds_2 = camels_sheds_2.rename(columns={'hru_id': 'gauge_id'})
    camels_sheds_2['gauge_id'] = camels_sheds_2['gauge_id'].astype(str).str.zfill(8)

    # first, testing a smaller subset
    camels_sheds_test = camels_sheds_2.head(10).copy()
    # print(camels_sheds_test)

    # empty list for watershed data
    camels_sheds_list = []

    # Loop through each row in the original GeoDataFrame
    for index, row in camels_sheds_test.iterrows():
        # Create a new GeoDataFrame with a single row
        single_row_gdf = camels_sheds_2.iloc[[index]]

        # Append it to the list
        camels_sheds_list.append(single_row_gdf)

    # Initialize a multiprocessing Pool
    with multiprocessing.Pool(processes=12) as pool:
        # Run the function in parallel using the Pool for each input region
        results = pool.map(nwi_iter, camels_sheds_list)
        print(results)



    # # WETLAND WORKFLOW, camels watersheds
    # # iterate data retrieval and metrics calculation for each camels watershed
    #
    # camels_sheds = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/CAMELS/basin_set_full_res/HCDN_nhru_final_671.shp')
    #
    # # first, testing a smaller subset
    # camels_sheds_test = camels_sheds.head(10).copy()
    # # print(camels_sheds_test)
    #
    # # Initialize an empty list to store the results
    # results = []
    #
    # for index, row in camels_sheds_test.iterrows():
    #     # create a geodataframe for current watershed
    #     current_shed = geopandas.GeoDataFrame([row], geometry='geometry', crs=camels_sheds_test.crs)
    #
    #     # removing unneeded attribute data and making sure ID column has 8 values (leading zero sometimes)
    #     shed_gdf = current_shed.loc[:, ['hru_id', 'geometry']]
    #     shed_gdf = shed_gdf.rename(columns={'hru_id': 'gauge_id'})
    #     shed_gdf['gauge_id'] = shed_gdf['gauge_id'].astype(str).str.zfill(8)
    #
    #     # data retrieval
    #     nwi_gdf = nwi_download_api(shed_gdf=shed_gdf, out_dir="E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles",
    #                                save=True)
    #
    #     # metrics processing
    #     nwi_area = calc_area_nwi(nwi_gdf)
    #     shed_area = calc_area_shed(shed_gdf)
    #     nwi_shed_join = wetlands_in_shed(nwi_area, shed_area)
    #     nwi_prep = prep_nwi(nwi_shed_join)
    #     nwi_metrics = calc_wetland_metrics(nwi_prep, shed_area)
    #
    #     # add to results
    #     results.append(nwi_metrics)
    #
    # result_gdf = pandas.concat(results, ignore_index=True)
    # result_gdf.to_file("E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/metrics_test.shp")
    # # print(result_gdf)





    # SIGNATURE WORKFLOW

    # # using CAMELS watersheds
    # camels_ids = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/CAMELS/camels_name.txt', delimiter=';')
    # # convert column with ids to string and add leading zeros if they were lost
    # camels_ids['gauge_id'] = camels_ids['gauge_id'].astype(str).str.zfill(8)
    # # print(camels_ids)
    #
    # # now want all gaugeids in GAges II reference dataset, except CAMELs
    # # downloaded basin ID file here: https://www.sciencebase.gov/catalog/item/59692a64e4b0d1f9f05fbd39
    # gages_II_ids = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/Gages-II/BasinID.txt', delimiter=',')
    # gages_II_ids['STAID'] = gages_II_ids['STAID'].astype(str).str.zfill(8)
    # # print(gages_II_ids)
    #
    # # now just get siteids not in camels list
    # siteids_list = list(set(gages_II_ids['STAID']) - set(camels_ids['gauge_id']))
    # # print(siteids_list)
    #
    # # downloading flow data and saving in format required for TOSSH toolbox processing
    # for num in range(3):
    #     siteid = siteids_list[num]
    #     print(siteid)
    #     out_dir_1 = "E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow"
    #     out_dir_2 = "E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow/mm_day"
    #
    #     drain_area = libs.dataretrieval_api.usgs_drain_area_download_api(siteid=siteid)
    #     print(drain_area)
    #     flow_cfs_df = libs.dataretrieval_api.usgs_daily_download_api(siteid=siteid,out_dir=out_dir_1, save=True)
    #     flow_mm_day_df = libs.dataretrieval_api.usgs_daily_prep(siteid=siteid, drain_area=drain_area,
    #                                                             flow_cfs_df=flow_cfs_df, out_dir=out_dir_2, save=True)


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
