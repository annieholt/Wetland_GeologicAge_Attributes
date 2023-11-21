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


# for now, working with BFI, rececession_a_seasonality, BaseflowRecessionK
# these only require flow data, and are recommended by McMillan et al., 2022

sigs_c_subset = sigs_c %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK) %>% 
  mutate(datasource = "CAMELS")

sigs_g_subset = sigs_g %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK) %>% 
  mutate(datasource = 'GagesII')


sigs = sigs_c_subset %>% 
  bind_rows(sigs_g_subset)


# more full dataset

sigs_c_2 = sigs_c %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope,
         MRC_num_segments, Spearmans_rho, VariabilityIndex) %>% 
  mutate(datasource = 'CAMELS')
sigs_g_2 = sigs_g %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope,
         MRC_num_segments, Spearmans_rho, VariabilityIndex) %>% 
  mutate(datasource = 'GagesII')


sigs_2 = sigs_c_2 %>% 
  bind_rows(sigs_g_2)


#### PREP WETLAND METRICS DATA ###

nwi_g = st_read('E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/nwi_gagesII_ref_metrics_new_ecoregions.shp') %>% 
  select(gauge_id, NA_L1KEY, shed_area, fresh, lake, other, geometry)
nwi_c = st_read('E:/SDSU_GEOG/Thesis/Data/NWI_outputs/Shapefiles/nwi_camels_metrics_ecoregions.shp') %>% 
  select(gauge_id,NA_L1KEY, shed_area, fresh, lake, other, geometry)

giws = st_read('E:/SDSU_GEOG/Thesis/Data/GIWs/giws_metrics.shp') %>% 
  as.data.frame() %>% 
  select(gauge_id, area_frac)

nwi_metrics = nwi_g %>% 
  bind_rows(nwi_c) %>% 
  left_join(giws, by = 'gauge_id') %>% 
  # some places have zero isolated wetlands, so replace NAs with zero since shapefiles weren't returned for those
  mutate(area_frac = ifelse(is.na(area_frac), 0, area_frac))

#### FINAL DATASET FOR PLOTTING AND ANALYSIS ####

sigs_nwi = sigs %>% 
  left_join(nwi_metrics, by = c('gauge_id'))


nwi_sigs = nwi_metrics %>% 
  left_join(sigs, by = c('gauge_id')) %>% 
  drop_na() %>% 
  mutate(fresh_total = fresh + lake) %>% 
  arrange(desc(area_frac))




#### trying some figures ####

conus <- st_read('E:/SDSU_GEOG/Thesis/Data/US states/conus_states.shp')

# Create a new sf object with points representing the centroids of the polygons
centroids_sf <- st_centroid(nwi_sigs)

# Plot the map with polygons represented as dots, colored by the variable
ggplot() +
  geom_sf(data = conus, color = "white", fill = "grey") +  # Plot polygon boundaries, color by variable
  geom_sf(data = centroids_sf, aes(fill = area_frac), shape = 21, color = "darkgrey", size = 4, alpha = 1) +  # Plot centroids as dots, color by variable
  # scale_fill_distiller(direction=1)+
  # scale_fill_viridis_c()+
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = NULL)+
  # scale_fill_gradient(low = "white", high = "blue4") +  # Set color scale
  ggtitle("Fresh Total") +
  theme_void()+
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),  # Adjust title size and centering
    legend.key.size = unit(2, "lines"),  # Adjust the size of the legend keys
    legend.title = element_text(size = 11),  # Adjust the size of the legend title
    legend.text = element_text(size = 10),# Adjust the size of the legend text
    legend.margin = margin(r = 10)
  )

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/giws.png", width = 11, height = 6, dpi = 300,bg = "white")


nwi_sigs_2 = nwi_metrics %>% 
  left_join(sigs_2, by = c('gauge_id')) %>% 
  drop_na() %>% 
  arrange(desc(area_frac))


nwi_sigs_categories = nwi_sigs_2 %>% 
  mutate(fresh_total = fresh + lake) %>% 
  mutate(fresh_category = case_when(fresh < 0.2 ~ 'class 1',
                                    fresh < 0.4 & fresh >= 0.2 ~ 'class 2',
                                    # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
                                    TRUE ~ 'class 3')) %>% 
  # mutate(total_fresh_category = case_when(fresh_total < 0.2 ~ 'class 1',
  #                                         fresh_total < 0.4 & fresh_total >= 0.2 ~ 'class 2',
  #                                   # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
  #                                   TRUE ~ 'class 3')) %>% 
  mutate(giw_category = case_when(area_frac < 0.04 ~ 'class 1',
                                  area_frac < 0.08 & area_frac >= 0.04 ~ 'class 2',
                                  # area_frac < 0.12 & area_frac >= 0.08 ~ 'class 3',
                                  TRUE ~ 'class 3')) %>% 
  st_drop_geometry() %>% 
  # filter(datasource == "CAMELS") %>%
  
  
  # # adjusting values based on McMillan et al. 2022 (based on exected distribution of GW signatures??)
  mutate(BaseflowRecessionK = ifelse(BaseflowRecessionK > 0.6, NA, BaseflowRecessionK)) %>%
  mutate(Recession_a_Seasonality = ifelse(Recession_a_Seasonality > 6, NA, Recession_a_Seasonality)) %>% 
  mutate(VariabilityIndex = ifelse(VariabilityIndex > 1, NA, VariabilityIndex)) %>% 
  mutate(Mid_Recession_Slope = ifelse(Mid_Recession_Slope > 0.6, NA, Mid_Recession_Slope)) %>% 
  mutate(First_Recession_Slope = ifelse(First_Recession_Slope > 1.5, NA, First_Recession_Slope))



nwi_sigs_categories_long = nwi_sigs_categories %>% 
  # filter(NA_L1KEY == "5  NORTHERN FORESTS") %>% 
  # select(giw_category, BFI, Recession_a_Seasonality, BaseflowRecessionK) %>% 
  select(fresh_category, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope, VariabilityIndex) %>%
  pivot_longer(!fresh_category, names_to = "signature")
  

# ggplot(nwi_sigs_categories, aes(x = fresh_category, y = BFI, fill = fresh_category)) +
#   geom_boxplot() +
#   ggtitle("Boxplots for Variable X by Category from Variable Y") +
#   xlab("Category from Variable Y") +
#   ylab("Variable X") +
#   theme_minimal()


# Create a set of boxplots for many different Y variables
ggplot(nwi_sigs_categories_long, aes(x = fresh_category, y = value, fill = fresh_category)) +
  geom_boxplot() +
  facet_wrap(~ signature, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("class 1" = "yellow", "class 2" = "green", "class 3" = "blue"),
                     name = str_wrap("Percent Wetlands", width = 16),
                     labels = c("0-20%", "20-40%", "40-80%")) +
  # ggtitle("Boxplots for Variable X by Different Y Variables") +
  xlab(NULL) +  # Remove X-axis label
  ylab(NULL) +  # Remove Y-axis label
  scale_x_discrete(
    breaks = c("class 1", "class 2", "class 3"),
    labels = c("0-20%", "20-40%", "40-80%")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    strip.text = element_text(size = 14, margin = margin(t = 5, b = 5)),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(vjust = 0, size = 12),  # Increase X-axis label size
    axis.text.y = element_text(size = 12),  # Increase Y-axis label size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/sig_boxplot_fresh_expanded.png", width = 11, height = 6, dpi = 300,bg = "white")

# # Plotting the histogram
# hist(nwi_sigs_categories$BaseflowRecessionK, col = "skyblue", main = "Histogram with Smooth Line", xlab = "Variable Values", ylab = "Frequency")
# # Adding a smooth line (density estimate)
# lines(density(nwi_sigs_categories$BaseflowRecessionK), col = "red", lwd = 2)


# Creating a ggplot with overlapping histograms for different categories
ggplot(nwi_sigs_categories_long, aes(x = value, color = fresh_category)) +
  geom_density(lwd = 2, alpha = 0.7) +  # Adjust line thickness with lwd and transparency with alpha
  facet_wrap(~ signature, scales = "free", ncol = 2, strip.position = "bottom") +
  scale_color_manual(values = c("class 1" = "yellow", "class 2" = "green", "class 3" = "blue"),
                     name = str_wrap("Percent Isolated Wetlands", width = 16),
                     labels = c("0-4%", "4-8%", "8-16%")) +  # Specify specific colors
  # ggtitle("Kernel Density Plot with Separate Lines for Different Categories") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    strip.text = element_text(size = 14, margin = margin(t = 5, b = 5)),  # Increase label size
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(vjust = 0, size = 16),  # Increase X-axis label size
    axis.text.y = element_blank(),
    legend.text = element_text(size = 16),  # Increase legend text size
    legend.title = element_text(size = 18)  # Increase legend title size
  )
# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/sig_distribution_giw_extended.png", width = 11, height = 8, dpi = 300,bg = "white")




#### Statistical testing ####

nwi_sigs_test <- nwi_sigs_categories %>% 
  filter(NA_L1KEY == "5  NORTHERN FORESTS") %>% 
  select(gauge_id, fresh, First_Recession_Slope) %>% 
  drop_na()
cor_result <- cor.test(nwi_sigs_test$fresh, nwi_sigs_test$First_Recession_Slope, method = "spearman", exact = FALSE)

# Print the correlation coefficient and p-value
cat("Spearman rank correlation coefficient:", cor_result$estimate, "\n")
cat("P-value:", cor_result$p.value, "\n")


ggplot(nwi_sigs_test, aes(x = fresh, y = First_Recession_Slope)) +
  geom_point(size = 4, color = 'black', shape = 21) +  # Increase the size of dots
  # geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add a smoothed line
  # ggtitle(paste("Spearman's Rank Correlation =", round(cor_result$estimate, 2),
  #               "p-value =", round(cor_result$p.value, 4))) +
  xlab("GIW Area Fraction") +
  ylab("BFI") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space below X-axis label
    axis.title.y = element_text(margin = margin(r = 10)),  # Increase space to the right of Y-axis label
    plot.margin = margin(20, 20, 20, 20)  # Adjust overall plot margin
  )

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/BFI_fresh.png", width = 4, height = 3, dpi = 300,bg = "white")





ggplot(nwi_sigs_test, aes(x = fresh, y = VariabilityIndex, fill = fresh_category)) +
  geom_boxplot() +
  ggtitle("Boxplots for Variable X by Category from Variable Y") +
  xlab("Category from Variable Y") +
  ylab("Variable X") +
  theme_minimal()

ggplot(nwi_sigs_test, aes(x = fresh, y = BFI)) +
  geom_point() +
  ggtitle("Boxplots for Variable X by Category from Variable Y") +
  xlab("Area Fraction") +
  ylab("BFI") +
  theme_minimal()



#### Five recommended signatures ####

sigs_c_5 = sigs_c %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK, TotalRR, AverageStorage)


nwi_sigs_5 = nwi_metrics %>% 
  left_join(sigs_c_5, by = c('gauge_id')) %>% 
  mutate(fresh_category = case_when(fresh < 0.2 ~ 'class 1',
                                    fresh < 0.4 & fresh >= 0.2 ~ 'class 2',
                                    # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
                                    TRUE ~ 'class 3')) %>% 
  # mutate(total_fresh_category = case_when(fresh_total < 0.2 ~ 'class 1',
  #                                         fresh_total < 0.4 & fresh_total >= 0.2 ~ 'class 2',
  #                                   # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
  #                                   TRUE ~ 'class 3')) %>% 
  mutate(giw_category = case_when(area_frac < 0.04 ~ 'class 1',
                                  area_frac < 0.08 & area_frac >= 0.04 ~ 'class 2',
                                  # area_frac < 0.12 & area_frac >= 0.08 ~ 'class 3',
                                  TRUE ~ 'class 3')) %>% 
  st_drop_geometry() %>% 
  drop_na(BFI) %>% 
  # # adjusting values based on McMillan et al. 2022 (based on exected distribution of GW signatures??)
  mutate(BaseflowRecessionK = ifelse(BaseflowRecessionK > 0.6, NA, BaseflowRecessionK)) %>%
  mutate(Recession_a_Seasonality = ifelse(Recession_a_Seasonality > 6, NA, Recession_a_Seasonality))


nwi_sigs_5_long = nwi_sigs_5 %>% 
  # filter(NA_L1KEY == "9  GREAT PLAINS") %>% 
  select(fresh_category,BFI, Recession_a_Seasonality, BaseflowRecessionK, TotalRR, AverageStorage) %>% 
  # select(fresh_category, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope, Spearmans_rho, VariabilityIndex) %>% 
  pivot_longer(!fresh_category, names_to = "signature")



ggplot(nwi_sigs_5_long, aes(x = fresh_category, y = value, fill = fresh_category)) +
  geom_boxplot() +
  facet_wrap(~ signature, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("class 1" = "yellow", "class 2" = "green", "class 3" = "blue"),
                    name = str_wrap("Percent Isolated Wetlands", width = 16),
                    labels = c("0-4%", "4-8%", "8-16%")) +
  # ggtitle("Boxplots for Variable X by Different Y Variables") +
  xlab(NULL) +  # Remove X-axis label
  ylab(NULL) +  # Remove Y-axis label
  scale_x_discrete(
    breaks = c("class 1", "class 2", "class 3"),
    labels = c("0-4%", "4-8%", "8-16%")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    strip.text = element_text(size = 14, margin = margin(t = 5, b = 5)),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(vjust = 0, size = 12),  # Increase X-axis label size
    axis.text.y = element_text(size = 12),  # Increase Y-axis label size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )


