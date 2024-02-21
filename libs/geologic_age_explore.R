# script to explore geology data a bit more

library(tidyverse)
library(sf)
library(ggplot2)
library(corrr)
library(broom)

##### looking at the raw SGMC data a bit ####

# geol_dir = "C:/Users/holta/Documents/ArcGIS_Projects/sgmc_explore/data"
# geol_file = "USGS_StateGeologicMapCompilation_withAge.csv"
# 
# # ages in million years ago
# # calculate average age, between min and max ages
# 
# geol_df = read_csv(paste(geol_dir, geol_file, sep = "/")) %>% 
#   mutate(AV_MA = (MIN_MA + MAX_MA)/2)
# 
# geol_sum = geol_df %>% 
#   # mutate(AV_MA = (MIN_MA + MAX_MA)/2) %>% 
#   group_by(GENERALIZED_LITH) %>%
#   summarize(
#     min_value = min(AV_MA),
#     max_value = max(AV_MA),
#     mean_value = mean(AV_MA),
#     median_value = median(AV_MA),
#     count = n()
#   )


##### SOME CORRELATION ANALYSIS and PLOTTING ####


#### PREP SIGNATURE DATA ###
sigs_c = read.csv('E:/SDSU_GEOG/Thesis/Data/Signatures/sigs_camels.csv', colClasses = c(gauge_id = "character"))


# signatures, no precip data needed
sigs_c_2 = sigs_c %>% 
  select(gauge_id, BFI, Recession_a_Seasonality, BaseflowRecessionK, First_Recession_Slope, Mid_Recession_Slope,
         MRC_num_segments, Spearmans_rho, VariabilityIndex, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c) %>% 
  mutate(datasource = 'CAMELS')

# groundwater signature set
# removed Storage Fraction as McMillan et al. 2022 found unreliable
sigs_c_3 = sigs_c %>% 
  select(gauge_id, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
         AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c, MRC_num_segments,
         First_Recession_Slope, Mid_Recession_Slope, Spearmans_rho, EventRR_TotalRR_ratio,
         VariabilityIndex, BFI, BaseflowRecessionK)


#### PREP GEOLOGY AGE METRICS DATA ###

# age by major geologic unit
geol_c = st_read('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/Shapefiles/sgmc_camels_metrics.shp')
# average age of catchment (age weighted by area of geologic unit type)
geol_c_av = st_read('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/Shapefiles/sgmc_camels_metrics_age_weighted.shp')


#### FINAL DATASET FOR PLOTTING AND ANALYSIS ####


geol_sigs = sigs_c_3 %>% 
  left_join(geol_c, by = c('gauge_id')) %>% 
  drop_na()
# why are there NAs???

geol_sigs_av = sigs_c_3 %>% 
  left_join(geol_c_av, by = c('gauge_id')) %>% 
  drop_na()
# why are there NAs???

#### SIGNATURE DISTRIBUTIONS AND PERCENTILES ####
# sanity checking signature calculations

sigs_c_3_long = sigs_c_3 %>% 
  pivot_longer(-gauge_id, names_to = "signature")

# Creating a ggplot of distributions
ggplot(sigs_c_3_long, aes(x = value)) +
  geom_density(lwd = 2, alpha = 0.7) +  # Adjust line thickness with lwd and transparency with alpha
  facet_wrap(~ signature, scales = "free", ncol = 4, strip.position = "bottom") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal()

# also percentile data for reference (maybe there are some outliers??)
sigs_percentiles = sigs_c_3_long %>% 
  group_by(signature) %>%
  summarize(quant25 = quantile(value, probs = 0.25, na.rm = TRUE), 
            quant50 = quantile(value, probs = 0.50, na.rm = TRUE),
            quant75 = quantile(value, probs = 0.75, na.rm = TRUE),
            quant90 = quantile(value, probs = 0.90, na.rm = TRUE))


#### CORRELATIONS #### 

corr_test = geol_sigs%>% 
  select(av_age, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
         AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c, MRC_num_segments,
         First_Recession_Slope, Mid_Recession_Slope, Spearmans_rho, EventRR_TotalRR_ratio,
         VariabilityIndex, BFI, BaseflowRecessionK) %>% 
  as.data.frame()
  # as.data.frame() %>%
  # correlate(method = "spearman") %>%
  # focus(av_age)

cor_results <- corr_test %>%
  gather(variable, value, -av_age) %>%
  group_by(variable) %>%
  do(tidy(cor.test(.$value, .$av_age, method = "spearman"))) %>%
  select(variable, estimate, p.value)
  

# cor_sigs <- cor.test(geol_sigs_av$av_age, geol_sigs_av$VariabilityIndex, method = "spearman", exact = FALSE)
# 
# # Print the correlation coefficient and p-value
# cat("Spearman rank correlation coefficient:", cor_sigs$estimate, "\n")
# cat("P-value:", cor_sigs$p.value, "\n")


# Create ggplot with colored and sized dots
ggplot(cor_results, aes(x = variable, y = 1, color = estimate, size = abs(estimate))) +
  geom_point() +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue", 
                        midpoint = mean(cor_results$estimate), name = "Spearman's Rho",
                        limits = c(-0.3, 0.3)) +
  scale_size(range = c(6, 16)) +  # Adjust the overall size scale
  labs(
    y = NULL,  # No y-axis label
    x = NULL,
  ) +
  ggtitle("Correlations with Major Lith Geologic Age") +  # Add plot title
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


# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_geology/geol_age_lith_spearmans.png", width = 10.5, height = 5, dpi = 300,bg = "white")




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



