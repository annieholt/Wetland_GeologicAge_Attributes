import os
import requests
from zipfile import ZipFile

# url for data download
url_pattern = "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/{}_geodatabase_wetlands.zip"

# path data is saved; for now, this is on a machine in the Data Viz lab
path_to_download = "C:/Users/aholt8450/Documents/Data/NWI_geodatabases"


# Function to generate download URL for each state
def generate_download_url(state_code):
    return url_pattern.format(state_code)


# Function to download and unzip data for a given state
def download_and_unzip_state_data(state_code):
    # Generate download URL for the state
    download_url = generate_download_url(state_code)

    # Create a directory for the state
    state_directory = os.path.join(path_to_download, state_code)
    os.makedirs(state_directory, exist_ok=True)

    # Download the data
    response = requests.get(download_url)
    zip_path = os.path.join(state_directory, "data.zip")
    with open(zip_path, 'wb') as zip_file:
        zip_file.write(response.content)

    # Unzip the data
    with ZipFile(zip_path, 'r') as zip_ref:
        zip_ref.extractall(state_directory)

    # Remove the zip file if needed
    os.remove(zip_path)


# List of state codes (replace with the actual state codes you are interested in)
state_codes = ["AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY",
               "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND",
               "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"]

# Loop through each state and download/unzip data
for state_code in state_codes:
    download_and_unzip_state_data(state_code)
