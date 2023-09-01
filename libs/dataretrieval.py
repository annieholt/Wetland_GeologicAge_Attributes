# Functions to download and extract NWI zip files
# NWI website: https://www.fws.gov/program/national-wetlands-inventory/data-download
# Includes function for single state or watershed download, or for multiple download iterations

import requests, zipfile, io, os

# Download single dataset, based on watershed or state URL retrieved from website
# Catch any errors (likely related to url formatting, or proper directory formatting)
# adapted from the following website tutorial: https://pythonguides.com/download-zip-file-from-url-using-python/
def nwi_download(url, out_dir):
    """
    :param url: URL to download zipped NWI shapefiles, specific to State or Watershed (string)
    :param out_dir: location to extract the zip file (string)
    :return: None
    """
    try:
        # create the output directory if one doesn't already exist
        os.makedirs(out_dir, exist_ok=True)
        # read in and store data files from url (which are initially zipped files)
        url_return = requests.get(url)
        # create zip file object from url content, and extract the data to local directory
        zip_data = zipfile.ZipFile(io.BytesIO(url_return.content))
        zip_data.extractall(out_dir)
        print("NWI data download and extraction complete.")
    except Exception as e:
        print(e)


# Download and extract multiple zip folders for states or watersheds, iterating the nwi_downlaod function
# leverages the patterns in the URL strings
def nwi_download_iter(loc_list, out_dir, download_by='state'):
    """
    :param loc_list: list of state abbreviations or huc* watershed codes to download
    :param out_dir: location to extract the zip file (string)
    :param download_by: type of area to download, either "state" or "watershed"; default is by state
    :return: None
    """
    # loop through each location in list
    for loc in loc_list:
        # create url based on input
        # the urls have repeating patterns, so adding the state abbrev or watershed to the pattern
        # example for state: https://www.fws.gov/wetlands/Data/State-Downloads/CA_shapefile_wetlands.zip
        # example for HUC8 shed: https://www.fws.gov/wetlands/downloads/Watershed/HU8_04010302_watershed.zip

        # the pattern depends on if downloading watershed or state, so first check that then create url pattern
        if download_by == 'state':
            # defining strings
            str_1 = 'https://www.fws.gov/wetlands/Data/State-Downloads/'
            # provided region, making sure is string type
            str_2 = str(loc)
            str_3 = '_shapefile_wetlands.zip'
            # combining strings
            url = str_1 + str_2 + str_3
            print('The following url was generated for the data download: {}'.format(url))
        elif download_by == 'watershed':
            # defining strings
            str_1 = 'https://www.fws.gov/wetlands/downloads/Watershed/HU8_'
            # provided region, making sure is string type
            str_2 = str(loc)
            str_3 = '_watershed.zip'
            # combining strings
            url = str_1 + str_2 + str_3
            print('The following url was generated for the data download: {}'.format(url))
        else:
            print('Download type (state or watershed) was not appropriately specified.')
            return None

        # use the generated url, run the download
        nwi_download(url=url, out_dir=out_dir)
