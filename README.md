# Wetland_GeologicAge_Attributes
Code and pipelines for downloading data from the National Wetland Inventory and State Geologic Map Compilation and generating watershed-scale wetland and geoloic age metrics.
Previous versions queried the datasets directly using ArcGIS API for Python, but due to server inconsistencies, newer code versions instead read in local datasets that were downloaded manually.

## main.py: workflows to call calculate_metrics functions and return shapefiles and new attributes for different catchment datasets

## calculate_metrics: functions to clip and summarize wetland and geology data/shapefiles using catchment boundaries
## dataretrieval_nwis: functions to retrieve streamflow data (used this when exploring signatures also in GAGES II catchments)

## r/geologic_age_correlation_analysis: correlation analysis and plotting between geologic age and hydrologic signatures (calculated from baseflow_prediction and TOSSH codes)
## r/wetland_correlation_analysis: correlation analysis and plotting between wetlands and hydrologic signatures (calculated from baseflow_prediction and TOSSH codes)
