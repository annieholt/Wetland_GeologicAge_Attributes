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



##### HYSETS data ####

# camels = st_read('E:/SDSU_GEOG/Thesis/Data/Caravan/shapefiles/camels/camels_basin_shapes_nad83.shp')
# camels_df = camels %>% as.data.frame() %>% select(gauge_id, gauge_id_2) %>% rename(gauge_id_camels = gauge_id)
# 
# hysets = st_read('E:/SDSU_GEOG/Thesis/Data/Caravan/shapefiles/hysets/hysets_basin_shapes_conus.shp')
# 
# hysets_2 = hysets %>% 
#   left_join(camels_df, by = c("gauge_id" = "gauge_id_2")) %>% 
#   filter(is.na(gauge_id_camels)) %>% 
#   select(gauge_id, geometry)
# 
# st_write(hysets_2, "E:/SDSU_GEOG/Thesis/Data/Caravan/shapefiles/hysets/hysets_basin_shapes_nocamels.shp")

##### SOME CORRELATION ANALYSIS and PLOTTING ####


#### PREP SIGNATURE DATA ###
sigs_c = read.csv('E:/SDSU_GEOG/Thesis/Data/Signatures/sigs_camels.csv', colClasses = c(gauge_id = "character"))


# signatures, no precip data needed
sigs_c_2 = sigs_c %>% 
  select(gauge_id,EventRR, TotalRR, RR_Seasonality, Recession_a_Seasonality, AverageStorage, RecessionParameters_a,
         RecessionParameters_b, RecessionParameters_c, First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
         VariabilityIndex, BaseflowRecessionK, BFI) %>% 
  rename(RecessionParameters_T0 = RecessionParameters_c)

# groundwater signature set
# removed Storage Fraction as McMillan et al. 2022 found unreliable
# sigs_c_3 = sigs_c %>% 
#   select(gauge_id, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
#          AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c, MRC_num_segments,
#          First_Recession_Slope, Mid_Recession_Slope, Spearmans_rho, EventRR_TotalRR_ratio,
#          VariabilityIndex, BFI, BaseflowRecessionK)


#### PREP GEOLOGY AGE METRICS DATA ###

# age by major geologic unit
geol_c = st_read('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/Shapefiles/sgmc_camels_metrics.shp')
# average age of catchment (age weighted by area of geologic unit type)
geol_c_av = st_read('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/Shapefiles/sgmc_camels_metrics_age_weighted.shp')


#### FINAL DATASET FOR PLOTTING AND ANALYSIS ####


geol_sigs = sigs_c_2 %>% 
  left_join(geol_c, by = c('gauge_id')) %>% 
  drop_na()
# why are there NAs???

geol_sigs_av = sigs_c_2 %>% 
  left_join(geol_c_av, by = c('gauge_id')) %>% 
  drop_na()
# why are there NAs???


geol_sigs = sigs_c_2 %>% 
  left_join(geol_c %>% select(-shed_area, -major_lith, -lith_area_), by = c('gauge_id')) %>% 
  left_join(geol_c_av %>% select(gauge_id, av_age_w), by = c('gauge_id', 'geometry'))


#### SIGNATURE DISTRIBUTIONS AND PERCENTILES ####
# sanity checking signature calculations

sigs_c_3_long = sigs_c_2 %>% 
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

corr_test = geol_sigs %>% 
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


#### MAPS ####

conus <- st_read('E:/SDSU_GEOG/Thesis/Data/US states/conus_states.shp')

# Create a new sf object with points representing the centroids of the polygons
centroids_sf <- st_centroid(geol_c)

# Plot the map with polygons represented as dots, colored by the variable
ggplot() +
  geom_sf(data = conus, color = "white", fill = "grey") +  # Plot polygon boundaries, color by variable
  geom_sf(data = centroids_sf, aes(fill = av_age), shape = 21, color = "darkgrey", size = 4, alpha = 1) +  # Plot centroids as dots, color by variable
  # scale_fill_distiller(direction=1)+
  # scale_fill_viridis_c()+
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = NULL)+
  # scale_fill_gradient(low = "white", high = "blue4") +  # Set color scale
  ggtitle("Geologic Age of Major Lithology (Ma)") +
  theme_void()+
  theme(
    plot.title = element_text(size = 30, hjust = 0.5),  # Adjust title size and centering
    legend.key.size = unit(2, "lines"),  # Adjust the size of the legend keys
    legend.title = element_text(size = 24),  # Adjust the size of the legend title
    legend.text = element_text(size = 24),# Adjust the size of the legend text
    legend.margin = margin(r = 10)
  )


ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_final/geol_age_lith_camels.png", width = 11, height = 6, dpi = 300,bg = "white")

# and major lithologies plot 
# ggplot() +
#   geom_sf(data = conus, color = "white", fill = "grey") +
#   geom_sf(data = centroids_sf, aes(fill = major_lith), shape = 21, color = "darkgrey", size = 4, alpha = 1) +
#   scale_fill_brewer(palette = "Set3", name = "Major Lithology") +
#   ggtitle("Dominant Lithologies") +
#   theme_void() +
#   theme(
#     plot.title = element_text(size = 30, hjust = 0.5),
#     legend.key.size = unit(2, "lines"),
#     legend.title = element_text(size = 24),
#     legend.text = element_text(size = 24)
#   )


#### CORRELATIONS ####

corr_av = geol_sigs %>% 
  select(av_age, EventRR, TotalRR, RR_Seasonality, Recession_a_Seasonality, AverageStorage, RecessionParameters_a,
         RecessionParameters_b, RecessionParameters_T0, First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
         VariabilityIndex, BaseflowRecessionK, BFI) %>% 
  as.data.frame() %>% 
  correlate(method = "spearman") %>% 
  focus(av_age)

corr_av_w = geol_sigs %>% 
  select(av_age_w, EventRR, TotalRR, RR_Seasonality, Recession_a_Seasonality, AverageStorage, RecessionParameters_a,
         RecessionParameters_b, RecessionParameters_T0, First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
         VariabilityIndex, BaseflowRecessionK, BFI) %>% 
  as.data.frame() %>% 
  correlate(method = "spearman") %>% 
  focus(av_age_w)

# prep for panel plotting
corr_sigs_geol = corr_av %>% 
  left_join(corr_av_w, by = "term") %>% 
  gather(key = "variable", value = "value", av_age, av_age_w)


# Define custom labels for facet_wrap
geol_labels <- c(
  "av_age" = "Average Geologic Age (Ma) of Major Lithology",
  "av_age_w" = "Average Geologic Age (Ma)"
)

ggplot(corr_sigs_geol, aes(x = term, y = 1, color = value, size = abs(value))) +
  geom_point() +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue", 
                        midpoint = mean(corr_sigs_geol$value), name = "Spearman's Rho",
                        limits = c(-0.3, 0.3)) +
  scale_size(range = c(6, 16)) +  # Adjust the overall size scale
  labs(
    y = NULL,  # No y-axis label
    x = NULL,
  ) +
  # ggtitle("Correlations with Isolated Wetland Area Fraction") +
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
  # coord_cartesian(ylim = c(1, 1))
  facet_wrap(~ variable, scales = "free_y", nrow = 2, labeller = as_labeller(geol_labels))


# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_final/sigs_geol_camels.png", width = 10.5, height = 6, dpi = 300,bg = "white")

