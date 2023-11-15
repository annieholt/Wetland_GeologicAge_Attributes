import requests
import pandas
import os

from datetime import datetime
import numpy

def usgs_daily_download_api(siteid, out_dir, save=False):
    """
    Function to download USGS daily discharge data, from USGS Daily Values Site Web REST Service
    :param siteid: site identifier, 8 digits long, as string
    :param out_dir: location to save dataset
    :param save: if true, save the dataset
    :return: dataframe with daily discharge values
    """

    # base URL of the USGS NWIS REST API
    base_url = "https://waterservices.usgs.gov/nwis/dv"

    # the parameters for the query
    # 1 Oct 1989 to 30 Sep 2009
    params = {
        "format": "json",
        "sites": siteid,
        "startDT": "1989-10-01",
        "endDT": "2009-09-30",
        "parameterCd": "00060"  # parameter code for daily discharge
    }

    try:
        # GET request to retrieve the data
        response = requests.get(base_url, params=params)
        response.raise_for_status()  # Raise an exception if there's an HTTP error

        # parse the JSON response
        data = response.json()

        # extract date and discharge values
        time_series = data["value"]["timeSeries"][0]["values"][0]["value"]

        # create a DataFrame of data
        q_df = pandas.DataFrame(time_series)
        # rename columns
        q_df.rename(columns={"dateTime": "date", "value": "q_daily_cfs"}, inplace=True)

        # convert date column to datetime format
        q_df["date"] = pandas.to_datetime(q_df["date"])

        # print(q_df)

        if save:
            # create output file path
            file_name = siteid + ".csv"

            # create full file path
            file_path = os.path.join(out_dir, file_name)
            # save the GeoDataFrame as csv
            q_df.to_csv(file_path, index=False)
            print(f"Downloaded data and saved as {file_path}")
        else:
            print("Dataframe not saved.")

        return q_df

    except requests.exceptions.RequestException as e:
        print("Failed to retrieve data:", str(e))
    except Exception as e:
        print("An error occurred:", str(e))


def usgs_drain_area_download_api(siteid):
    """
    Function to download USGS daily discharge data, from USGS Daily Values Site Web REST Service.
    Based on function readNWISsite.R in dataRetrieval Package (https://github.com/DOI-USGS/dataRetrieval/blob/HEAD/R/readNWISsite.R).
    :param siteid: site identifier, 8 digits long, as string
    :return: a value of the basin drainage area
    """

    url = f"https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&format=rdb&sites={siteid}"
    # read data directly, which returns all the expanded site info

    try:
        data = pandas.read_csv(url, sep='\t', comment='#', header=0)

        # repalce '.' with NaN in columns containing '_va'
        va_columns = [col for col in data.columns if '_va' in col]
        data[va_columns] = data[va_columns].replace('.', pandas.NA)

        # Convert columns containing '_va' to numeric
        data[va_columns] = data[va_columns].apply(pandas.to_numeric, errors='coerce')

        # extract drainage area value from second row
        drain_area_va = data['drain_area_va'].iloc[1]

        # convert miles to mm2
        drain_area_mm2 = drain_area_va * (1609.34 ** 2) * (1000 ** 2)

        return drain_area_mm2

    except requests.exceptions.RequestException as e:
        print("Failed to retrieve data:", str(e))
        return None
    except Exception as e:
        print("An error occurred:", str(e))
        return None


# should function return matlab file format instead??
def usgs_daily_prep(siteid, flow_cfs_df, drain_area, out_dir, save=False):
    """
    Function to prep USGS daily flow data for the TOSSH Toolbox. units are converted to mm/day, time series is filled
    out with NaNs, negative flow values are converted to NaN, date ranges are finalized.
    :param siteid: site identifier, 8 digits long, as string
    :param flow_cfs_df: dataframe of downloaded USGS flow data (columns 'date', 'qualifiers', 'q_daily_cfs')
    :param drain_area: value of drainage area for the site, in square milimeters
    :param out_dir: location to save dataset
    :return: dataframe of USGS discharge data, including flow values, dates, and qualifiers
    """

    flow_df = flow_cfs_df
    try:
        # Filtering data within the date range
        # making sure flow values are numeric format
        # flow_df = flow_df[(flow_df['date'] >= '1989-10-01') & (flow_df['date'] <= '2009-09-30')]
        flow_df['q_daily_cfs'] = pandas.to_numeric(flow_df['q_daily_cfs'], errors='coerce')

        # Calculating flow in mm/day, required for TOSSH
        flow_df['q_mm_day'] = flow_df['q_daily_cfs'] * 60 * 60 * 24 * (1 / 3.28084) ** 3 * (1000 ** 3) * (
                1 / drain_area)

        # Filling gaps with NaNs
        start_date = flow_df['date'].min()
        end_date = flow_df['date'].max()
        # print(start_date, end_date)

        if start_date.month >= 10:
            start_date_final = datetime(start_date.year, 10, 1)
        else:
            start_date_final = datetime(start_date.year - 1, 10, 1)

        if end_date.month >= 10:
            end_date_final = datetime(end_date.year + 1, 9, 30)
        else:
            end_date_final = datetime(end_date.year, 9, 30)

        # Convert the start and end dates back to the desired format ('%Y-%m-%d')
        start_date_str = start_date_final.strftime('%Y-%m-%d')
        end_date_str = end_date_final.strftime('%Y-%m-%d')

        date_range = pandas.date_range(start_date_str, end_date_str)
        # print(date_range)

        # Create a DataFrame with all dates in the range
        date_df = pandas.DataFrame({'date': date_range})

        # Merge the original data with the full date range DataFrame to fill gaps
        flow_df_full = date_df.merge(flow_df, on='date', how='left')
        # print(flow_df_full)

        # Replace negative Q.mm.day values with 0
        flow_df_full['q_mm_day'] = numpy.where(flow_df_full['q_mm_day'] < 0, 0, flow_df_full['q_mm_day'])

        # Format datetime
        flow_df_full['datetime'] = flow_df_full['date'].dt.strftime('%d-%b-%Y')

        # final columns for the export
        final_columns = ['datetime', 'q_mm_day', 'qualifiers']

        # Create a new DataFrame with only the specific columns
        flow_df_final = flow_df_full[final_columns]

        if save:
            # create output file path
            file_name = siteid + ".csv"

            # create full file path
            file_path = os.path.join(out_dir, file_name)
            # save the GeoDataFrame as csv
            flow_df_final.to_csv(file_path, index=False)
            print(f"Downloaded data and saved as {file_path}")
        else:
            print("Dataframe not saved.")

        return flow_df_final

    except Exception as e:
        print("An error occurred:", str(e))
        return None

# # testing functions, on one USGS site
# test_out = usgs_drain_area_download_api(siteid="03049646")
# # test_df = usgs_daily_download_api(siteid="02322800",
# #                                   out_dir="C:/Users/holta/Documents/ArcGIS_Projects/wetland_metrics/Data", save=False)
# test_df = usgs_daily_download_api(siteid="03049646",
#                                   out_dir="C:/Users/aholt8450/Documents/Data/usgs_streamflow", save=True)
# # test_df_prepped = usgs_daily_prep(siteid="02322800", drain_area=test_out, flow_cfs_df=test_df,
# #                                   out_dir="C:/Users/holta/Documents", save=True)
# test_df_prepped = usgs_daily_prep(siteid="03049646", drain_area=test_out, flow_cfs_df=test_df,
#                                   out_dir="C:/Users/aholt8450/Documents/Data/usgs_streamflow/mm_day", save=True)

# test_flow = pandas.read_csv('E:/SDSU_GEOG/Thesis/Data/Gages-II/usgs_streamflow_2/mm_day/01021470.csv')
# print(test_flow)
#
# test_flow.replace('', pandas.NA, inplace=True)
# # Convert columns to numeric (optional)
# test_flow['Column1'] = pandas.to_numeric(test_flow['q_mm_day'], errors='coerce')
# print(test_flow)