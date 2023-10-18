# Workflow for CAMELS and GAGES II Catchments
import libs.dataretrieval_api
# first downloading and prepping flow data
# then running MatLab TOSSH toolbox functions to generated groundwater signatures
# next, download NWI data for catchments and generate wetland metrics
# finally, analyze the relationships

# importing module libraries
from libs.dataretrieval_api import *


def main():
    # using CAMELS watersheds
    camels_ids = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/CAMELS/camels_name.txt', delimiter=';')
    # convert column with ids to string and add leading zeros if they were lost
    camels_ids['gauge_id'] = camels_ids['gauge_id'].astype(str).str.zfill(8)
    # print(camels_ids)

    # now want all gaugeids in GAges II reference dataset, except CAMELs
    # downloaded basin ID file here: https://www.sciencebase.gov/catalog/item/59692a64e4b0d1f9f05fbd39
    gages_II_ids = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/Gages-II/BasinID.txt', delimiter=',')
    gages_II_ids['STAID'] = gages_II_ids['STAID'].astype(str).str.zfill(8)
    # print(gages_II_ids)

    # now just get siteids not in camels list
    siteids_list = list(set(gages_II_ids['STAID']) - set(camels_ids['gauge_id']))
    # print(siteids_list)

    # downloading flow data and saving in format required for TOSSH toolbox processing
    for num in range(3):
        siteid = siteids_list[num]
        print(siteid)
        out_dir_1 = "E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow"
        out_dir_2 = "E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow/mm_day"

        drain_area = libs.dataretrieval_api.usgs_drain_area_download_api(siteid=siteid)
        print(drain_area)
        flow_cfs_df = libs.dataretrieval_api.usgs_daily_download_api(siteid=siteid,out_dir=out_dir_1, save=True)
        flow_mm_day_df = libs.dataretrieval_api.usgs_daily_prep(siteid=siteid, drain_area=drain_area,
                                                                flow_cfs_df=flow_cfs_df, out_dir=out_dir_2, save=True)


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
