# Script to analyze and plot signatures vs wetland area

library(tidyverse)
library(sf)
library(ggplot2)
library(corrr)

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
         MRC_num_segments, Spearmans_rho, VariabilityIndex, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c) %>% 
  mutate(datasource = 'CAMELS')
sigs_g_2 = sigs_g %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope,
         MRC_num_segments, Spearmans_rho, VariabilityIndex, RecessionParameters_1, RecessionParameters_2, RecessionParameters_3) %>% 
  mutate(datasource = 'GagesII') %>% 
  rename(RecessionParameters_a = RecessionParameters_1, RecessionParameters_b = RecessionParameters_2,
         RecessionParameters_c = RecessionParameters_3)


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
  mutate(area_frac = ifelse(is.na(area_frac), 0, area_frac)) %>% 
  mutate(fresh_no_giw = fresh + lake - area_frac)

#### FINAL DATASET FOR PLOTTING AND ANALYSIS ####

sigs_nwi = sigs %>% 
  left_join(nwi_metrics, by = c('gauge_id'))


nwi_sigs = nwi_metrics %>% 
  left_join(sigs, by = c('gauge_id')) %>% 
  drop_na() %>% 
  mutate(fresh_total = fresh + lake) %>% 
  arrange(desc(area_frac))


nwi_sigs_2 = nwi_metrics %>% 
  left_join(sigs_2, by = c('gauge_id')) %>% 
  drop_na() %>% 
  mutate(fresh_total = fresh + lake) %>% 
  arrange(desc(area_frac))

nwi_sigs_camels = nwi_metrics %>% 
  select(gauge_id,area_frac, fresh_no_giw, geometry) %>% 
  left_join(sigs_c, by = c('gauge_id')) %>% 
  filter(!is.na(gauge_lat))


#### wetland correlations #### 

ggplot(nwi_sigs_2, aes(x = fresh_no_giw, y = area_frac)) +
  geom_point(size = 4, color = 'black', shape = 21) +  # Increase the size of dots
  # geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add a smoothed line
  # ggtitle(paste("Spearman's Rank Correlation =", round(cor_result$estimate, 2),
  #               "p-value =", round(cor_result$p.value, 4))) +
  xlab("Wetland Area Fraction") +
  ylab("GIWs") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space below X-axis label
    axis.title.y = element_text(margin = margin(r = 10)),  # Increase space to the right of Y-axis label
    plot.margin = margin(20, 20, 20, 20)  # Adjust overall plot margin
  )

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/BaseflowRecessionK_fresh.png", width = 4, height = 3, dpi = 300,bg = "white")


cor_wet <- cor.test(nwi_sigs_2$fresh_no_giw, nwi_sigs_2$area_frac, method = "spearman", exact = FALSE)

# Print the correlation coefficient and p-value
cat("Spearman rank correlation coefficient:", cor_wet$estimate, "\n")
cat("P-value:", cor_wet$p.value, "\n")


# cor_spearman <- cor(nwi_sigs_2$fresh_no_giw, nwi_sigs_2$area_frac, method = "spearman")
# 
# # Scatter plot
# plot(nwi_sigs_2$fresh_no_giw, nwi_sigs_2$area_frac, main = "Scatter Plot with Spearman Correlation", 
#      xlab = "Variable 1", ylab = "Variable 2", pch = 16, col = "blue")
# text(x = max(nwi_sigs_2$fresh_no_giw), y = max(nwi_sigs_2$area_frac), 
#      label = paste("Spearman Correlation =", round(cor_spearman, 2)), pos = 4)


corr_test = nwi_sigs_2 %>% 
  select(fresh_no_giw, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope,
         MRC_num_segments, VariabilityIndex, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c) %>% 
  as.data.frame() %>% 
  correlate(method = "spearman") %>% 
  focus(fresh_no_giw) %>% 
  mutate(term_2 = factor(term, levels = term[order(fresh_no_giw)]))




cor_sigs <- cor.test(nwi_sigs_2$area_frac, nwi_sigs_2$VariabilityIndex, method = "spearman", exact = FALSE)

# Print the correlation coefficient and p-value
cat("Spearman rank correlation coefficient:", cor_sigs$estimate, "\n")
cat("P-value:", cor_sigs$p.value, "\n")






corr_test_subregion = nwi_sigs_2 %>% 
  filter(NA_L1KEY == "5  NORTHERN FORESTS") %>% 
  select(fresh_no_giw, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope,
         MRC_num_segments, VariabilityIndex, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c) %>% 
  as.data.frame() %>% 
  correlate(method = "spearman") %>% 
  focus(fresh_no_giw) %>% 
  mutate(term_2 = factor(term, levels = term[order(fresh_no_giw)]))
  

# ggplot(corr_test, aes(x = term_2, y = fresh_no_giw)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   labs(
#     y = "Spearman Rank Correlation Coefficient",
#     x = "Hydrologic Signature"
#   ) +
#   theme_minimal() +                    # Use a minimal theme
#   theme(
#     panel.grid = element_blank(),     # Remove background grid lines
#     text = element_text(size = 12),   # Increase text (axis labels, title) size
#     axis.title = element_text(size = 14)  # Increase axis title size
#   ) +
#   coord_cartesian(ylim = c(-0.25, 0.25))


# Create ggplot with colored and sized dots
ggplot(corr_test, aes(x = term, y = 1, color = fresh_no_giw, size = abs(fresh_no_giw))) +
  geom_point() +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue", 
                        midpoint = mean(corr_test$fresh_no_giw), name = "Spearman's Rho",
                        limits = c(-0.3, 0.3)) +
  scale_size(range = c(6, 16)) +  # Adjust the overall size scale
  labs(
    y = NULL,  # No y-axis label
    x = NULL,
  ) +
  ggtitle("Correlations with Connected Wetland Area Fraction") +  # Add plot title
  theme_minimal() +                    # Use a minimal theme
  theme(
    plot.title = element_text(size=24),
    text = element_text(size = 20),   # Increase text (axis labels, title) size
    axis.title = element_text(size = 14),  # Increase axis title sizehttp://127.0.0.1:42413/graphics/plot_zoom_png?width=619&height=258
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels diagonally
    axis.text.y = element_blank(),  # Remove y-axis values
    legend.position = "left",  # Move legend to the left side
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.key.size = unit(2, "lines")  # Adjust the size of the legend color key
  ) +
  guides(size = FALSE)+
  coord_cartesian(ylim = c(1, 1))


# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/fresh_no_giw_spearmans.png", width = 10.5, height = 5, dpi = 300,bg = "white")




#### camels correlations ####

corr_test_camels = nwi_sigs_camels %>% 
  select(-geometry, -gauge_id, -gauge_lon, -gauge_lat, -area_frac) %>% 
  as.data.frame() %>% 
  correlate(method = "spearman") %>% 
  focus(fresh_no_giw) %>% 
  mutate(term_2 = factor(term, levels = term[order(fresh_no_giw)]))

ggplot(corr_test_camels, aes(x = term, y = 1, color = fresh_no_giw, size = abs(fresh_no_giw))) +
  geom_point() +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue", 
                        midpoint = mean(corr_test_camels$fresh_no_giw), name = "Spearman's Rho",
                        limits = c(-1, 1)) +
  scale_size(range = c(2, 14)) +  # Adjust the overall size scale
  labs(
    y = NULL,  # No y-axis label
    x = NULL,
  ) +
  ggtitle("Correlations with Non-Isolated Wetland Area Fraction (CAMELS only)") +  # Add plot title
  theme_minimal() +                    # Use a minimal theme
  theme(
    plot.title = element_text(size=12),
    text = element_text(size = 12),   # Increase text (axis labels, title) size
    axis.title = element_text(size = 14),  # Increase axis title sizehttp://127.0.0.1:42413/graphics/plot_zoom_png?width=619&height=258
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels diagonally
    axis.text.y = element_blank(),  # Remove y-axis values
    legend.position = "left",  # Move legend to the left side
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.key.size = unit(1, "lines")  # Adjust the size of the legend color key
  ) +
  guides(size = FALSE)+
  coord_cartesian(ylim = c(1, 1))

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/fresh_no_giw_camels_spearmans.png", width = 8, height = 3, dpi = 300,bg = "white")



#### trying some figures ####

conus <- st_read('E:/SDSU_GEOG/Thesis/Data/US states/conus_states.shp')

# Create a new sf object with points representing the centroids of the polygons
centroids_sf <- st_centroid(nwi_metrics)

# Plot the map with polygons represented as dots, colored by the variable
ggplot() +
  geom_sf(data = conus, color = "white", fill = "grey") +  # Plot polygon boundaries, color by variable
  geom_sf(data = centroids_sf, aes(fill = area_frac), shape = 21, color = "darkgrey", size = 4, alpha = 1) +  # Plot centroids as dots, color by variable
  # scale_fill_distiller(direction=1)+
  # scale_fill_viridis_c()+
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = NULL)+
  # scale_fill_gradient(low = "white", high = "blue4") +  # Set color scale
  ggtitle("Isolated Wetland Area Fraction") +
  theme_void()+
  theme(
    plot.title = element_text(size = 30, hjust = 0.5),  # Adjust title size and centering
    legend.key.size = unit(2, "lines"),  # Adjust the size of the legend keys
    legend.title = element_text(size = 24),  # Adjust the size of the legend title
    legend.text = element_text(size = 24),# Adjust the size of the legend text
    legend.margin = margin(r = 10)
  )

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/nwi_giw.png", width = 11, height = 6, dpi = 300,bg = "white")



nwi_sigs_categories = nwi_sigs_2 %>% 
  # mutate(fresh_total = fresh + lake) %>% 
  mutate(fresh_category = case_when(fresh_no_giw < 0.1 ~ 'class 1',
                                    fresh_no_giw < 0.3 & fresh_no_giw >= 0.1 ~ 'class 2',
                                    # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
                                    TRUE ~ 'class 3')) %>% 
  # mutate(total_fresh_category = case_when(fresh_total < 0.2 ~ 'class 1',
  #                                         fresh_total < 0.4 & fresh_total >= 0.2 ~ 'class 2',
  #                                   # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
  #                                   TRUE ~ 'class 3')) %>% 
  mutate(giw_category = case_when(area_frac < 0.01 ~ 'class 1',
                                  area_frac < 0.05 & area_frac >= 0.01 ~ 'class 2',
                                  # area_frac < 0.12 & area_frac >= 0.08 ~ 'class 3',
                                  TRUE ~ 'class 3')) %>% 
  st_drop_geometry() %>% 
  # filter(datasource == "CAMELS") %>%
  
  
  # # adjusting values based on McMillan et al. 2022 (based on exected distribution of GW signatures??)
  mutate(BaseflowRecessionK = ifelse(BaseflowRecessionK > 0.6, NA, BaseflowRecessionK)) %>%
  mutate(Recession_a_Seasonality = ifelse(Recession_a_Seasonality > 6, NA, Recession_a_Seasonality)) %>%
  # mutate(VariabilityIndex = ifelse(VariabilityIndex > 1, NA, VariabilityIndex)) %>%
  mutate(Mid_Recession_Slope = ifelse(Mid_Recession_Slope > 0.8, NA, Mid_Recession_Slope)) %>%
  mutate(First_Recession_Slope = ifelse(First_Recession_Slope > 2, NA, First_Recession_Slope)) %>% 
  mutate(RecessionParameters_b = ifelse(RecessionParameters_b > 8, NA, RecessionParameters_b)) %>% 
  mutate(RecessionParameters_c = ifelse(RecessionParameters_c > 100, NA, RecessionParameters_c))



nwi_sigs_categories_long = nwi_sigs_categories %>% 
  # filter(NA_L1KEY == "5  NORTHERN FORESTS") %>% 
  # select(giw_category, BFI, Recession_a_Seasonality, BaseflowRecessionK) %>% 
  # select(fresh_category, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope, VariabilityIndex) %>%
  select(giw_category, BFI, BaseflowRecessionK, VariabilityIndex, First_Recession_Slope, RecessionParameters_b,
         RecessionParameters_c) %>%
  pivot_longer(!giw_category, names_to = "signature")
  

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
                     name = str_wrap("Percent Non-Isolated Wetlands", width = 16),
                     labels = c("0-20%", "20-40%", "40-80%")) +
  # ggtitle("Boxplots for Variable X by Different Y Variables") +
  xlab(NULL) +  # Remove X-axis label
  ylab(NULL) +  # Remove Y-axis label
  scale_x_discrete(
    breaks = c("class 1", "class 2", "class 3"),
    labels = c("0-10%", "10-30%", "> 30%")
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
ggplot(nwi_sigs_categories_long, aes(x = value, color = giw_category)) +
  geom_density(lwd = 2, alpha = 0.7) +  # Adjust line thickness with lwd and transparency with alpha
  facet_wrap(~ signature, scales = "free", ncol = 2, strip.position = "bottom") +
  scale_color_manual(values = c("class 1" = "darkgrey", "class 2" = "orange", "class 3" = "purple"),
                     name = str_wrap("Percent Isolated Wetlands", width = 16),
                     labels = c("0-1%", "1-5%", "> 5%")) +  # Specify specific colors
  # ggtitle("Kernel Density Plot with Separate Lines for Different Categories") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    strip.text = element_text(size = 24, margin = margin(t = 5, b = 5)),  # Increase label size
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(vjust = 0, size = 16),  # Increase X-axis label size
    axis.text.y = element_blank(),
    legend.text = element_text(size = 26),  # Increase legend text size
    legend.title = element_text(size = 26)  # Increase legend title size
  )

ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/sig_distribution_giw_extended_2.png", width = 11, height = 8, dpi = 300,bg = "white")




#### Statistical testing ####

nwi_sigs_test <- nwi_sigs_categories %>% 
  filter(NA_L1KEY == "5  NORTHERN FORESTS") %>%
  select(gauge_id, fresh, VariabilityIndex) %>%
  drop_na()
cor_result <- cor.test(nwi_sigs_test$fresh, nwi_sigs_test$VariabilityIndex, method = "spearman", exact = FALSE)

# Print the correlation coefficient and p-value
cat("Spearman rank correlation coefficient:", cor_result$estimate, "\n")
cat("P-value:", cor_result$p.value, "\n")


ggplot(nwi_sigs_test, aes(x = fresh, y = BaseflowRecessionK)) +
  geom_point(size = 4, color = 'black', shape = 21) +  # Increase the size of dots
  # geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add a smoothed line
  # ggtitle(paste("Spearman's Rank Correlation =", round(cor_result$estimate, 2),
  #               "p-value =", round(cor_result$p.value, 4))) +
  xlab("Wetland Area Fraction") +
  ylab("BaseflowRecessionK") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space below X-axis label
    axis.title.y = element_text(margin = margin(r = 10)),  # Increase space to the right of Y-axis label
    plot.margin = margin(20, 20, 20, 20)  # Adjust overall plot margin
  )

ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/Figures/BaseflowRecessionK_fresh.png", width = 4, height = 3, dpi = 300,bg = "white")





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


