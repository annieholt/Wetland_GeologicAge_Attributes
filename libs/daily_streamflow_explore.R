# Script to explore USGS streamflow data at the reference watersheds
# will eventually move this workflow over to python functions

# this seemed like a useful site also: https://bcgov.github.io/fasstr/articles/fasstr_dataRetrieval.html

library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(R.matlab)


#### DATA DOWNLOAD ####

# Try the code below with the site.code here, then use the site code for your watershed.
site.code = "01607500"  #  The USGS stream gage code

what.data = whatNWISdata(siteNumber = as.character(site.code))

what.data.discharge = what.data %>% 
  filter(parm_cd == "00060")

site.info <- readNWISsite(site.code)
drain.area.mi2 = site.info$drain_area_va
drain.area.mm2 = drain.area.mi2 * (1609.34^2) * (1000^2)

# drain.area.mi2
# drainge.area.ft2 = drain.area.mi2*(5280^2)

# Download discharge data at this location
parameter.code = "00060"  # this is the code for stream discharge.
start = ""  # Blanks get all of the data
end = ""
flow.df = readNWISdv(site.code,parameter.code,start,end)
head(flow.df)
flow.df.daily = readNWISdv(site.code,parameter.code,start,end)


#### DATA PREPARATION ####

# generally want all data in between 1989 and 2009, which is same time period as camels
flow.df.2 = flow.df %>% 
  rename(Q.cfs = X_00060_00003, Q.cfs.cd = X_00060_00003_cd) %>% 
  filter(Date >= as.Date("1989-10-01")) %>% 
  filter(Date <= as.Date("2009-09-30")) %>% 
  mutate(Q.mm.day = Q.cfs * 60 * 60 * 24 * (1/3.28084)^3 * (1000^3) * (1/drain.area.mm2))

# checking an example of data completeness based on water year
flow.df.explore = flow.df.2 %>% 
  mutate(month = month(Date)) %>%
  mutate(year = year(Date)) %>% 
  mutate(wy = case_when(month < 10 ~ year, 
                        month >= 10 ~ year + 1)) %>% 
  group_by(wy) %>% 
  summarize(count = n())

# visualizing time series
plot(flow.df.2$Date,flow.df.2$Q.mm.day,xlab="Date",ylab="Daily Discharge, mm/day", type = 'l')



# now filling in data gaps with NANs, as per TOSSH Toolbox requirements
# want to do this dynamically, and flexible to datasets that have smaller time periods
# therefore, assigning start and end dates to full data through based on data

start.month = month(flow.df.2$Date[1])
start.year = year(flow.df.2$Date[1])
start.date = if (start.month >= 10) {
  as.Date(paste(start.year, "10", "01", sep = "-"))
} else {
  as.Date(paste(start.year - 1, "10", "01", sep = "-"))
}

end.month = month(flow.df.2$Date[nrow(flow.df.2)])
end.year =  year(flow.df.2$Date[nrow(flow.df.2)])
end.date = if (end.month >= 10) {
  as.Date(paste(end.year + 1, "09", "30", sep = "-"))
} else {
  as.Date(paste(end.year, "09", "30", sep = "-"))
}

# first creating dataframe of dates
Date.full <- seq(start.date, end.date, by = "day")
Date.full.df = as.data.frame(Date.full)

# joining data to date dataframe so that time series is filled in
flow.df.full = Date.full.df %>% 
  left_join(flow.df.2, by = c("Date.full" = "Date")) %>% 
  # mutate(Q.mm.day = case_when(is.na(Q.mm.day) ~ 'NaN',
  #                          TRUE ~ as.character(Q.mm.day))) %>% 
  mutate(Q.mm.day = case_when(Q.mm.day < 0 ~ 0,
                           TRUE ~ Q.mm.day)) %>%
  mutate(datetime = format(Date.full, "%d-%b-%Y"))


## checking data quality codes???
# https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd
# A is approved, P is provisional, Ae is approved but edited, etc.


# checking if all water years will filled in with rows
flow.df.full.explore = flow.df.full %>% 
  mutate(month = month(Date.full)) %>%
  mutate(year = year(Date.full)) %>% 
  mutate(wy = case_when(month < 10 ~ year, 
                        month >= 10 ~ year + 1)) %>% 
  group_by(wy) %>% 
  summarize(count = n())


# want at least 5 years of data??


#### DATA EXPORT ####

# try exporting as matlab data file, with datetime and Q, for signature functions

# tryCatch({
#   # Uniquely named
#   writeMat("C:/Users/holta/Documents/matdata_test_2.mat", datetime = as.matrix(flow.df.full$datetime),
#            Q = as.matrix(flow.df.full$Q.mm.day))
# }, error = function(ex) {
#   cat("ERROR:", ex$message, "\n")
# })



#### GAP ANALYSIS ####

# idea from Trent...
# every year, count number of NAs in flow timeseries
# report chart... on axes as gague ID, another as year/percent completeness
# then color by that percent completeness green to red
