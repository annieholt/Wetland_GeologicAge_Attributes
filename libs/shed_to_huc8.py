# Functions to download the Watershed Boundary Dataset (WBD) for HUC8 watersheds nationally
# Functions to then align CAMELS watersheds with relevant HUC8 watersheds and retain the HUC8 codes

# Note that CAMELS watersheds have different boundaries than HUC8 watersheds, but knowing the relevant HUC8 codes
# will enable more efficient querying of National Wetland Inventory Dataset
# (as shapefiles are downloadable by HUC8 codes)

# EXTRA thoughts...
# Original idea was to try calling functions from the R package nhdplusTools, which has data access tools
# information is available at https://doi-usgs.github.io/nhdplusTools/index.html
# then, use example workflow such as this to guide calling the R functions in Python scripts:
# https://willfondrie.com/2022/01/how-to-use-r-packages-in-python/
# however, nhdplusTools not available for install in Anaconda Environment??

# Next looked at PyNHD package, which based partially off of nhdplusTools
# information and code viewable at: https://pypi.org/project/pynhd/
# resources like this might be useful for querying data for a particular area of interest
# # but just need the national seamless WBD data for this national scale analyses

# Decided to just download whole WBD dataset hosted on Amazon Web services, available as Geodatabase

import os, requests, zipfile, io, geopandas


def wbd_download(out_dir):
    """
    :param out_dir: location to extract the zip files (string)
    :return: None
    """
    try:
        # create the output directory if one doesn't already exist
        os.makedirs(out_dir, exist_ok=True)
        # read in and store data files from url (which are initially zipped files)
        url = 'https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip'
        url_return = requests.get(url)
        # create zip file object from url content, and extract the data to local directory
        zip_data = zipfile.ZipFile(io.BytesIO(url_return.content))
        zip_data.extractall(out_dir)
        print("WBD data download and extraction complete.")
    except Exception as e:
        print(e)

# this takes several minutes
# wbd_download(out_dir='E:/SDSU_GEOG/Thesis/Data/WBD_Test')


def shed_to_huc8(camles_shed_gdf, wbd_huc8_gdf):
    """
    Function to intake a watershed boundary and retain HUC8 watersheds that make up/intersect with input watershed
    :param camles_shed_gdf: geodataframe of watershed(s) of interest. for current project this is one or more CAMELS watersheds
    :param wbd_huc8_gdf: geodataframe of HUC8 Watershed Boundary Dataset, downloaded using wbs_download function
    :return: geodataframe of all huc8 watersheds that overlap with watershed of interest.
    """
    # convert to same CRS
    camles_alb = camles_shed_gdf.to_crs(epsg=5070)
    huc8_alb = wbd_huc8_gdf.to_crs(epsg=5070)

    # retain all huc8 watersheds where there are CAMELS watersheds
    huc8_camels = geopandas.sjoin(huc8_alb, camles_alb, op='intersects', how='inner')
    # checking column names
    print(huc8_camels.columns.tolist())

    # drop duplicate huc8 watershed boundaries
    # might remove this step, but basically this removes redundant data downloads in the future
    huc8_camels_nodup = huc8_camels.drop_duplicates(subset=['tnmid'])

    # retaining only a few data columns (
    huc8_camels_final = huc8_camels_nodup[['geometry', 'huc8', 'hru_id']]
    print(huc8_camels_final)

# testing
# huc8_gdf = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/WBD_Test/WBD_National_GDB.gdb', layer='WBDHU8')
# print(huc8_gdf)
#
# # using CAMELS watersheds
# camels = geopandas.read_file('E:/SDSU_GEOG/Thesis/Data/CAMELS/basin_set_full_res/HCDN_nhru_final_671.shp')
# # need to add leading zeros to gauge ids, so 8 total digits
# # first convert to string, then add zero
# camels['hru_id'] = camels['hru_id'].astype(str).str.zfill(8)
