# Script to analyze and plot signatures vs wetland area

library(tidyverse)
library(sf)
library(ggplot2)

# Gages II signature data appending

# sig_dir_1 = 'E:/SDSU_GEOG/Thesis/Data/Signatures/gages_II'
# 
# file_list_1 = list.files(path = sig_dir_1, pattern = "*.csv", full.names = TRUE)
# 
# sigs_1 = lapply(file_list_1, read_csv) %>% 
#   bind_rows()
# 
# write.csv(sigs_1, 'E:/SDSU_GEOG/Thesis/Data/Signatures/sigs_gagesII_ref_subet.csv', row.names = FALSE)

#### PREP SIGNATURE DATA ###
sigs_c = read.csv('E:/SDSU_GEOG/Thesis/Data/Signatures/sigs_camels.csv', colClasses = c(gauge_id = "character"))
sigs_g = read.csv('E:/SDSU_GEOG/Thesis/Data/Signatures/sigs_gagesII_ref_subset.csv', colClasses = c(gauge_id = "character")) %>% 
  mutate(gauge_id = str_pad(string = as.numeric(gauge_id), width = 8, side = 'left', pad = 0))

x <- str_pad(as.(sigs_g$gauge_id), width = 8, side = "left", pad = "0")

# for now, working with BFI, rececession_a_seasonality, BaseflowRecessionK
# these only require flow data, and are recommended by McMillan et al., 2022

sigs_c_subset = sigs_c %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK) %>% 
  mutate(datasource = "CAMELS")

sigs_g_subset = sigs_g %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK) %>% 
  mutate(datasource = 'GagesII')


sigs = sigs_c_subset %>% 
  bind_rows(sigs_g_subset) %>% 
  drop_na()


#### PREP WETLAND METRICS DATA ###

nwi_g = st_read('E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/nwi_gagesII_ref_metrics_new_ecoregions.shp') %>% 
  select(gauge_id, NA_L1KEY, shed_area, fresh, lake, other, geometry)
nwi_c = st_read('E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/nwi_camels_metrics_ecoregions.shp') %>% 
  select(gauge_id,NA_L1KEY, shed_area, fresh, lake, other, geometry)

nwi_metrics = nwi_g %>% 
  bind_rows(nwi_c)

#### FINAL DATASET FOR PLOTTING AND ANALYSIS ####

sigs_nwi = sigs %>% 
  left_join(nwi_metrics, by = c('gauge_id'))

test_1 = sigs_nwi %>%
  # filter(datasource == 'CAMELS') %>% 
  filter(NA_L1KEY == "5  NORTHERN FORESTS") %>% 
  select(BFI, fresh) %>% 
  drop_na()

test = cor.test(test_1$BFI, test_1$fresh, method = "spearman")
print(test)


# plot(test_1$BFI, test_1$fresh, col=test_1$NA_L1KEY)

# ggplot(test_1,aes(x=BFI,y=fresh,col=NA_L1KEY))+geom_point()
ggplot(test_1,aes(x=BFI,y=fresh))+geom_point()





test_2 = sigs_nwi %>%
  filter(NA_L1KEY == "8  EASTERN TEMPERATE FORESTS") %>% 
  select(BaseflowRecessionK, fresh) %>% 
  drop_na()

test_cor_2 = cor.test(test_2$BaseflowRecessionK, test_2$fresh, method = "spearman")
print(test)


# plot(test_1$BFI, test_1$fresh, col=test_1$NA_L1KEY)

# ggplot(test_1,aes(x=BFI,y=fresh,col=NA_L1KEY))+geom_point()
ggplot(test_2,aes(x=BaseflowRecessionK,y=fresh))+geom_point()


