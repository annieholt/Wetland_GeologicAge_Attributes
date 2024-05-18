# script to explore geology data, and analyze relationship between new geologic age metrics and signatures

library(tidyverse)
library(sf)
library(ggplot2)
library(corrr)
library(broom)
library(cowplot)

##### looking at the raw SGMC data a bit ####

geol_dir = "C:/Users/holta/Documents/ArcGIS_Projects/sgmc_explore/data"
geol_file = "USGS_StateGeologicMapCompilation_withAge.csv"

# ages in million years ago
# calculate average age, between min and max ages

geol_df = read_csv(paste(geol_dir, geol_file, sep = "/")) %>%
  mutate(AV_MA = (MIN_MA + MAX_MA)/2)

geol_sum = geol_df %>%
  # mutate(AV_MA = (MIN_MA + MAX_MA)/2) %>%
  group_by(GENERALIZED_LITH) %>%
  summarize(
    min_value = min(AV_MA),
    max_value = max(AV_MA),
    mean_value = mean(AV_MA),
    median_value = median(AV_MA),
    count = n()
  )

geol_df$GENERALIZED_LITH <- factor(geol_df$GENERALIZED_LITH, 
                                   levels = names(sort(tapply(geol_df$AV_MA, geol_df$GENERALIZED_LITH, mean), decreasing = TRUE)))


ggplot(geol_df, aes(x = GENERALIZED_LITH, y = AV_MA)) +
  geom_boxplot(position = "dodge") +
  labs(
    y = "Average Geologic Age (Ma)",
    x = "Lithological Classification of Major Geologic Unit",
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 24),
    text = element_text(size = 20),   # Increase text (axis labels, title) size
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels diagonally
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    legend.position = "left",  # Move legend to the left side
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.key.size = unit(2, "lines")  # Adjust the size of the legend color key
  )


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


#### PREP SIGNATURE DATA ####
sigs_c = read.csv('E:/SDSU_GEOG/Thesis/Data/Signatures/sigs_camels_v2.csv', colClasses = c(gauge_id = "character"))


# signatures, no precip data needed
# sigs_c_2 = sigs_c %>% 
#   select(gauge_id, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
#          AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c, MRC_num_segments,
#          First_Recession_Slope, Mid_Recession_Slope, Spearmans_rho, EventRR_TotalRR_ratio,
#          VariabilityIndex, BFI, BFI_90, BaseflowRecessionK) %>% 
#   as.data.frame() %>% 
#   rename(RecessionParameters_T0 = RecessionParameters_c)

# groundwater signature set
# removed Storage Fraction as McMillan et al. 2022 found unreliable
# sigs_c_3 = sigs_c %>% 
#   select(gauge_id, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
#          AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c, MRC_num_segments,
#          First_Recession_Slope, Mid_Recession_Slope, Spearmans_rho, EventRR_TotalRR_ratio,
#          VariabilityIndex, BFI, BaseflowRecessionK)

# final set
sigs_c_2 = sigs_c %>%
  select(gauge_id, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
         AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_c,
         First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
         VariabilityIndex, BFI, BFI_90, BaseflowRecessionK) %>%
  as.data.frame() %>%
  rename(RecessionParameters_T0 = RecessionParameters_c)


#### IMPORT/PREP GEOLOGY AGE METRICS DATA ###

# age by major geologic unit
geol_c_lith = st_read('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/Shapefiles/sgmc_camels_metrics.shp')
# average age of catchment (age weighted by area of geologic unit type)
geol_c_av = st_read('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/Shapefiles/sgmc_camels_metrics_age_weighted.shp')

# geol_c$major_lith <- factor(geol_c$major_lith, 
#                                    levels = names(sort(tapply(geol_c$av_age, geol_c$major_lith, mean), decreasing = TRUE)))


# just exploring age by major lithology relationship....
ggplot(geol_c_lith, aes(x = major_lith, y = av_age)) +
  geom_boxplot(position = "dodge") +
  labs(
    y = "Average Geologic Age (Ma)",
    x = "Lithological Classification of Major Geologic Unit",
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 24),
    text = element_text(size = 20),   # Increase text (axis labels, title) size
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels diagonally
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    legend.position = "left",  # Move legend to the left side
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.key.size = unit(2, "lines")  # Adjust the size of the legend color key
  )




#### FINAL DATASET FOR PLOTTING AND ANALYSIS ####


# geol_sigs = sigs_c_2 %>% 
#   left_join(geol_c, by = c('gauge_id')) %>% 
#   drop_na()
# # why are there NAs???
# 
# geol_sigs_av = sigs_c_2 %>% 
#   left_join(geol_c_av, by = c('gauge_id')) %>% 
#   drop_na()
# # why are there NAs???


geol_sigs = sigs_c_2 %>% 
  left_join(geol_c_lith %>% select(-shed_area, -major_lith, -lith_area_), by = c('gauge_id')) %>% 
  left_join(geol_c_av %>% select(gauge_id, av_age_w), by = c('gauge_id', 'geometry')) %>% 
  rename(av_age_lith = av_age) %>% 
  rename(av_age = av_age_w)

geol_sigs_lith= sigs_c_2 %>%
  left_join(geol_c_lith %>% select(-shed_area, -lith_area_), by = c('gauge_id')) %>%
  left_join(geol_c_av %>% select(gauge_id, av_age_w), by = c('gauge_id', 'geometry')) %>% 
  rename(av_age_lith = av_age) %>% 
  rename(av_age = av_age_w)

#### SIGNATURE DISTRIBUTIONS AND PERCENTILES ####
# sanity checking signature calculations

sigs_c_3_long = sigs_c_2 %>% 
  select(-RecessionParameters_a) %>% 
  # select(gauge_id, AverageStorage, BaseflowRecessionK, BFI, BFI_90, Recession_a_Seasonality, TotalRR) %>%
  pivot_longer(-gauge_id, names_to = "signature")

# Creating a ggplot of distributions
ggplot(sigs_c_3_long, aes(x = value)) +
  geom_density(lwd = 2, alpha = 0.7) +
  facet_wrap(~ signature, scales = "free", ncol = 4, strip.position = "bottom") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal()


# now plotting, but with specific axes limits

# create dataframe, with signature and min/max values

signature = c('EventRR', 'TotalRR', 'RR_Seasonality', 'Recession_a_Seasonality', 'AverageStorage',
              'RecessionParameters_b', 'RecessionParameters_T0',
                          'First_Recession_Slope', 'Mid_Recession_Slope','EventRR_TotalRR_ratio',
                          'VariabilityIndex', 'BaseflowRecessionK',
                          'BFI', 'BFI_90')
x_max = c(1, 1, 5, 6, 550, 6, 60, 2, 1, 1, 1, 0.5, 1, 1)
sig_limits = data.frame(signature, x_max)

# List to store individual scatterplots
sig_scatterplot_list <- list()

# Loop through each variable
for (sig in unique(sigs_c_3_long$signature)) {
  
  # filter dataset by sig
  df = sigs_c_3_long[sigs_c_3_long$signature == sig, ]
  x_max_df = sig_limits[sig_limits$signature == sig, ]
  x_max = x_max_df$x_max
  print(x_max)
  
  # Create scatterplot for current variable
  scatterplot <- ggplot(df, aes(x = value)) +
    geom_density(lwd = 2, alpha = 0.7) +
    ggtitle(sig) +
    xlim(0, x_max) +
    theme_bw()+
    theme(axis.title = element_blank(),
          axis.text.y =element_blank(),
          axis.ticks.y =element_blank())
  
  # Add scatterplot to list
  sig_scatterplot_list[[sig]] <- scatterplot
}

# Combine scatterplots into a single plot
sig_combined_plot <- cowplot::plot_grid(plotlist = sig_scatterplot_list, nrow = 3)

# Print the combined plot
print(sig_combined_plot)

ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_final/sig_distributions_v2.png", width = 16, height = 8, dpi = 300,bg = "white")
# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_v2/sig_distributions.png", width = 7, height = 4, dpi = 300,bg = "white")




# also percentile data for reference (maybe there are some outliers??)
sigs_percentiles = sigs_c_3_long %>% 
  group_by(signature) %>%
  summarize(quant25 = quantile(value, probs = 0.25, na.rm = TRUE), 
            quant50 = quantile(value, probs = 0.50, na.rm = TRUE),
            quant75 = quantile(value, probs = 0.75, na.rm = TRUE),
            quant90 = quantile(value, probs = 0.90, na.rm = TRUE))


#### CORRELATIONS, single geologic age metric at a time #### 

corr_test = geol_sigs %>% 
  select(av_age_lith, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
         AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_T0,
         First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
         VariabilityIndex, BFI, BFI_90, BaseflowRecessionK) %>% 
  as.data.frame()
  # as.data.frame() %>%
  # correlate(method = "spearman") %>%
  # focus(av_age)


cor_results <- corr_test %>%
  gather(variable, value, -av_age_lith) %>%
  group_by(variable) %>%
  do(tidy(cor.test(.$value, .$av_age_lith, method = "spearman"))) %>%
  select(variable, estimate, p.value) %>% 
  mutate(p.value = format(p.value, scientific = FALSE))

write.csv(cor_results, "E:/SDSU_GEOG/Thesis/Data/Signatures/figures_v2/sig_av_ag_lith_corr.csv")


corr_lith = geol_sigs_lith %>% 
  filter(grepl('Sedimentary, carbonate', major_lith)) %>%
  # filter(!grepl('Igneous|Sedimentary, clastic', major_lith))
  select(av_age, TotalRR, RR_Seasonality, EventRR, Recession_a_Seasonality,
         AverageStorage, RecessionParameters_a, RecessionParameters_b, RecessionParameters_T0,
         First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
         VariabilityIndex, BFI, BFI_90, BaseflowRecessionK) %>% 
  select(av_age, TotalRR,Recession_a_Seasonality,
         AverageStorage, BFI, BFI_90, BaseflowRecessionK) %>% 
  as.data.frame()
  

cor_results_lith <- corr_lith %>%
  gather(variable, value, -av_age) %>%
  group_by(variable) %>%
  do(tidy(cor.test(.$value, .$av_age, method = "spearman"))) %>%
  select(variable, estimate, p.value) %>% 
  mutate(p.value = format(p.value, scientific = FALSE))



# Create ggplot with colored and sized dots
ggplot(cor_results_lith, aes(x = variable, y = 1, color = estimate, size = abs(estimate))) +
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", 
                        midpoint = mean(cor_results$estimate), name = "Spearman's Rho",
                        limits = c(-0.7, 0.7)) +
  scale_size(range = c(6, 16)) +  # Adjust the overall size scale
  labs(
    y = NULL,  # No y-axis label
    x = NULL,
  ) +
  ggtitle("Correlations with Geologic Age, Sedimentary carbonate") +  # Add plot title
  theme_minimal() +                    # Use a minimal theme
  theme(
    plot.title = element_text(size=16),
    # plot.title = element_text(size=24),
    text = element_text(size = 20),   # Increase text (axis labels, title) size
    axis.title = element_text(size = 14),  # Increase axis title sizehttp://127.0.0.1:42413/graphics/plot_zoom_png?width=619&height=258
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels diagonally
    axis.text.y = element_blank(),  # Remove y-axis values
    legend.position = "left",  # Move legend to the left side
    legend.title = element_text(size = 14),
    # legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.key.size = unit(2, "lines")  # Adjust the size of the legend color key
  ) +
  guides(size = FALSE)+
  coord_cartesian(ylim = c(1, 1))

ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_v2/geol_age_sedimentary_carb_spearmans.png", width = 8, height = 5, dpi = 300,bg = "white")

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_geology/geol_age_sedimentary_spearmans.png", width = 10.5, height = 5, dpi = 300,bg = "white")

#### MAPS ####

conus <- st_read('E:/SDSU_GEOG/Thesis/Data/US states/conus_states.shp')

geol_hysets = st_read('E:/SDSU_GEOG/Thesis/Data/Geology_outputs/Hysets/sgmc_hysets_metrics_age_majorlith.shp')

geol_all = geol_c %>% 
  bind_rows(geol_hysets)


# Create a new sf object with points representing the centroids of the polygons
# centroids_sf <- st_centroid(geol_c)
centroids_sf <- st_centroid(geol_all)

# Plot the map with polygons represented as dots, colored by the variable
ggplot() +
  geom_sf(data = conus, color = "white", fill = "grey") +  # Plot polygon boundaries, color by variable
  geom_sf(data = centroids_sf, aes(fill = av_age), shape = 21, color = "darkgrey", size = 3, alpha = 1) +  # Plot centroids as dots, color by variable
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
    legend.text = element_text(size = 24)# Adjust the size of the legend text
  )


# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_final/geol_age_majorlith_all.png", width = 11, height = 6, dpi = 300,bg = "white")


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


#### CORRELATIONS, BOTH METRICS on same plot ####

cor_geol <- cor.test(geol_sigs$av_age, geol_sigs$av_age_lith, method = "spearman")

corr_av = geol_sigs %>% 
  # select(av_age, EventRR, TotalRR, RR_Seasonality, Recession_a_Seasonality, AverageStorage, RecessionParameters_a,
  #        RecessionParameters_b, RecessionParameters_T0, First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
  #        VariabilityIndex, BaseflowRecessionK, BFI, BFI_90) %>% 
  select(av_age, TotalRR, Recession_a_Seasonality, AverageStorage,BaseflowRecessionK, BFI, BFI_90) %>% 
  as.data.frame() %>% 
  correlate(method = "spearman") %>% 
  focus(av_age)

corr_av_lith = geol_sigs %>% 
  # select(av_age_lith, EventRR, TotalRR, RR_Seasonality, Recession_a_Seasonality, AverageStorage, RecessionParameters_a,
  #        RecessionParameters_b, RecessionParameters_T0, First_Recession_Slope, Mid_Recession_Slope, EventRR_TotalRR_ratio,
  #        VariabilityIndex, BaseflowRecessionK, BFI, BFI_90) %>% 
  select(av_age_lith, TotalRR, Recession_a_Seasonality, AverageStorage,BaseflowRecessionK, BFI, BFI_90) %>% 
  as.data.frame() %>% 
  correlate(method = "spearman") %>% 
  focus(av_age_lith)

# prep for panel plotting
corr_sigs_geol = corr_av %>% 
  left_join(corr_av_lith, by = "term") %>% 
  gather(key = "variable", value = "value", av_age, av_age_lith)


# Define custom labels for facet_wrap
geol_labels <- c(
  "av_age" = "Average Geologic Age (Ma)",
  "av_age_lith" = "Average Geologic Age (Ma) of Major Lithology"
)

ggplot(corr_sigs_geol, aes(x = term, y = 1, color = value, size = abs(value))) +
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", 
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
# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_v2/sigs_geol_camels.png", width = 8, height = 6, dpi = 300,bg = "white")



#### LINEAR REGRESSIONS?? just for more testing ####

#  Linear model, is there a trend in total annual runoff?
lm_test = lm(geol_sigs$BFI ~ geol_sigs$av_age)
summary(lm_test)

# adding trendline
plot(geol_sigs$av_age,geol_sigs$BFI,ylab="BFI",xlab="average age")
abline(lm_test)

hist(log(geol_sigs$av_age_lith))


# plot residuals
lm_test_stdres = rstandard(lm_test)
qqnorm(lm_test_stdres, ylab="Standardized Residuals", xlab="Normal Scores") 
qqline(lm_test_stdres)
hist(lm_test$residuals)

# test for normality 
shapiro.test(lm_test$residuals)





#### SCATTERPLOTS, BOXPLOTS, HISTOGRAMS ####


camels_attribs = read.csv("E:/SDSU_GEOG/Thesis/Data/RandomForest_R/inputs/camels_attribs_addor18.csv",
                          colClasses = c(gauge_id = "character"))
camels_eco = read.csv("E:/SDSU_GEOG/Thesis/Data/RandomForest_R/inputs/camels_ecoregions.csv", colClasses = c(gauge_id = "character"))


# for zooming in on relationships....

geol_sigs_camels_plotting = geol_sigs_lith %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  select(gauge_id, av_age, av_age_lith, major_lith, TotalRR, Recession_a_Seasonality, AverageStorage,BaseflowRecessionK, BFI, BFI_90) %>% 
  # filter(grepl('Igneous', major_lith)) %>%
  filter(major_lith == "Igneous, volcanic" | major_lith == "Sedimentary, carbonate") %>%
  # select(-major_lith)
  filter(major_lith == "Sedimentary, carbonate") %>%
  # filter(av_age < 40) %>%
  left_join(camels_attribs %>% select(gauge_id, geol_porostiy, geol_permeability, carbonate_rocks_frac), by = "gauge_id") %>% 
  select(gauge_id, av_age, av_age_lith, major_lith, carbonate_rocks_frac, geol_porostiy, geol_permeability, BFI, BFI_90) 


# geol_sigs_camels_plotting = geol_sigs_lith %>% 
#   as.data.frame() %>% 
#   select(-geometry) %>% 
#   select(gauge_id, av_age, av_age_lith, major_lith, TotalRR, Recession_a_Seasonality, AverageStorage,BaseflowRecessionK, BFI, BFI_90) %>% 
#   left_join(camels_eco, by = "gauge_id") %>% 
#   filter(major_lith == "Igneous, volcanic" | major_lith == "Sedimentary, clastic") %>%
#   filter(major_lith == "Sedimentary, clastic") %>%
#   filter(av_age < 500) %>%
#   # filter(NA_L1KEY =="8  EASTERN TEMPERATE FORESTS") %>%
#   select(gauge_id, av_age, av_age_lith, major_lith,NA_L1KEY,BFI, BFI_90)

# Reshape the data into long format
geol_sigs_camels_long <- pivot_longer(geol_sigs_camels_plotting, cols = c(-av_age_lith,-av_age, -major_lith,-gauge_id, -carbonate_rocks_frac,
                                                                          -geol_permeability, -geol_porostiy), names_to = "signature", values_to = "sig_value")

# Reshape the data into long format
# geol_sigs_camels_long <- pivot_longer(geol_sigs_camels_plotting, cols = c(-av_age_lith,-av_age, -major_lith,-gauge_id, -NA_L1KEY), names_to = "signature", values_to = "sig_value")

# Create scatterplot with facetting
geol_sigs_scatterplot <- ggplot(geol_sigs_camels_long, aes(x = av_age, y = sig_value, fill = geol_permeability)) +
  geom_point(size = 4, shape = 24, color = "black", stroke = 0.5) +  # Increase the size of dots
  # facet_wrap(~ signature, scales = "free", ncol = 5) +  # Facet by the variable
  facet_wrap(~ signature, scales = "free", ncol = 3) +  # Facet by the variable
  xlab("Average Geologic Age") +
  ylab("Signature Value") +
  # scale_color_gradient(limits = c(0, 0.28), low = "orange", high = "blue") +
  scale_fill_gradient(limits = c(-17, -10), low = "red", high = "blue")+  
  labs(fill = "Permeability (log10)") +
  # scale_x_continuous(limits = c(0, 24))+
  theme_minimal()
  # theme(legend.position = "none")
  # theme(
  #   axis.title.x = element_text(margin = margin(t = 10)),  # Increase space below X-axis label
  #   axis.title.y = element_text(margin = margin(r = 10)),  # Increase space to the right of Y-axis label
  #   plot.margin = margin(20, 20, 20, 20)  # Adjust overall plot margin
  # )

# Print the scatterplot
print(geol_sigs_scatterplot)

ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_v2/geol_age_BFI_igneous_scatterplot.png", width = 10, height = 4, dpi = 300,bg = "white")



# Create scatterplot with facetting
geol_sigs_scatterplot <- ggplot(geol_sigs_camels_long, aes(x = av_age, y = sig_value, color = major_lith)) +
  geom_point(size = 4, shape = 21) +  # Increase the size of dots
  # facet_wrap(~ signature, scales = "free", ncol = 5) +  # Facet by the variable
  facet_wrap(~ signature, scales = "free", ncol = 3) +  # Facet by the variable
  xlab("Average Geologic Age") +
  ylab("Signature") +
  # scale_x_continuous(limits = c(0, 24))+
  theme_minimal()
  # theme(
  #   axis.title.x = element_text(margin = margin(t = 10)),  # Increase space below X-axis label
  #   axis.title.y = element_text(margin = margin(r = 10)),  # Increase space to the right of Y-axis label
  #   plot.margin = margin(20, 20, 20, 20)  # Adjust overall plot margin
  # )

# Print the scatterplot
print(geol_sigs_scatterplot)



# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_final/geol_age_sig_scatterplot.png", width = 16, height = 8, dpi = 300,bg = "white")

ggplot(geol_sigs_camels_long, aes(x = sig_value, color = major_lith)) +
  geom_density(lwd = 2, alpha = 0.7) +  # Adjust line thickness with lwd and transparency with alpha
  # facet_wrap(~ signature, scales = "free", ncol = 5, strip.position = "bottom") +
  facet_wrap(~ signature, scales = "free", ncol = 3, strip.position = "bottom") +
  
  # scale_color_manual(values = c("class 1" = "darkgrey", "class 2" = "orange", "class 3" = "purple"),
  #                    name = str_wrap("Percent Isolated Wetlands", width = 16),
  #                    labels = c("< 0.1%", "0.1-1%", ">1%")) +  # Specify specific colors
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    strip.text = element_text(size = 11, margin = margin(t = 5, b = 5)),  # Increase label size
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(vjust = 0, size = 10),  # Increase X-axis label size
    axis.text.y = element_blank(),
    legend.text = element_text(size = 11),  # Increase legend text size
    legend.title = element_text(size = 11)  # Increase legend title size
  )

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_v2/major_lith_sig_distributions.png", width = 8, height = 4, dpi = 300,bg = "white")



ggplot(geol_sigs_camels_long, aes(y = geol_permeability, x = major_lith)) +
  geom_boxplot() +
  # facet_wrap(~ signature, scales = "free", ncol = 3, strip.position = "bottom") +
  xlab(NULL) +
  ylab("Average Geologic Age (Ma)") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    strip.text = element_text(size = 14, margin = margin(t = 5, b = 5)),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_text(vjust = 0, size = 12),  # Increase X-axis label size
    axis.text.y = element_text(size = 12)  # Increase Y-axis label size
  )

# ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_v2/major_lith_age_distributions.png", width = 8, height = 4, dpi = 300,bg = "white")




## BOXPLOTS ##
# can adapt the code for wetland metrics....?

# nwi_sigs_camels_cat = nwi_sigs_camels %>% 
#   mutate(fresh_category = case_when(fresh_no_giw < 0.01 ~ 'class 1',
#                                     fresh_no_giw < 0.1 & fresh_no_giw >= 0.01 ~ 'class 2',
#                                     # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
#                                     TRUE ~ 'class 3')) %>% 
#   # mutate(total_fresh_category = case_when(fresh_total < 0.2 ~ 'class 1',
#   #                                         fresh_total < 0.4 & fresh_total >= 0.2 ~ 'class 2',
#   #                                   # fresh < 0.6 & fresh >= 0.4 ~ 'class 3',
#   #                                   TRUE ~ 'class 3')) %>% 
#   mutate(giw_category = case_when(area_frac < 0.001 ~ 'class 1',
#                                   area_frac < 0.01 & area_frac >= 0.0001 ~ 'class 2',
#                                   # area_frac < 0.12 & area_frac >= 0.08 ~ 'class 3',
#                                   TRUE ~ 'class 3')) %>% 
#   st_drop_geometry()
# 
# 
# nwi_sigs_camels_cat_long = nwi_sigs_camels_cat %>% 
#   pivot_longer(c(-giw_category, -fresh_category, -fresh_no_giw, -area_frac, -gauge_id), names_to = "signature")
# 
# 
# ggplot(nwi_sigs_camels_cat_long, aes(x = giw_category, y = value, fill = giw_category)) +
#   geom_boxplot() +
#   facet_wrap(~ signature, scales = "free_y", ncol = 5) +
#   # scale_fill_manual(values = c("class 1" = "yellow", "class 2" = "green", "class 3" = "blue"),
#   #                   name = str_wrap("Percent Connected Wetlands", width = 16),
#   #                   labels = c("< 1%", "1-10%", "> 10%")) +
#   scale_fill_manual(values = c("class 1" = "yellow", "class 2" = "green", "class 3" = "blue"),
#                     name = str_wrap("Percent Isolated Wetlands", width = 16),
#                     labels = c("< 0.1%", "0.1-1%", ">1%")) +
#   # ggtitle("Boxplots for Variable X by Different Y Variables") +
#   xlab(NULL) +  # Remove X-axis label
#   ylab(NULL) +  # Remove Y-axis label
#   # scale_x_discrete(
#   #   breaks = c("class 1", "class 2", "class 3"),
#   #   labels = c("< 1%", "1-10%", "> 10%")
#   # ) +
#   scale_x_discrete(
#     breaks = c("class 1", "class 2", "class 3"),
#     labels = c("< 0.1%", "0.1-1%", ">1%")
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(),
#     strip.text = element_text(size = 14, margin = margin(t = 5, b = 5)),
#     strip.background = element_blank(),
#     strip.placement = "outside",
#     axis.text.x = element_text(vjust = 0, size = 12),  # Increase X-axis label size
#     axis.text.y = element_text(size = 12),  # Increase Y-axis label size
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 14)
#   )
# 
# ggplot(nwi_sigs_camels_cat_long, aes(x = value, color = fresh_category)) +
#   geom_density(lwd = 2, alpha = 0.7) +  # Adjust line thickness with lwd and transparency with alpha
#   facet_wrap(~ signature, scales = "free", ncol = 5, strip.position = "bottom") +
#   # scale_color_manual(values = c("class 1" = "darkgrey", "class 2" = "orange", "class 3" = "purple"),
#   #                    name = str_wrap("Percent Isolated Wetlands", width = 16),
#   #                    labels = c("< 0.1%", "0.1-1%", ">1%")) +  # Specify specific colors
#   scale_color_manual(values = c("class 1" = "darkgrey", "class 2" = "orange", "class 3" = "purple"),
#                      name = str_wrap("Percent Connected Wetlands", width = 16),
#                      labels = c("< 1%", "1-10%", "> 10%")) +  # Specify specific colors
#   # ggtitle("Kernel Density Plot with Separate Lines for Different Categories") +
#   xlab(NULL) +
#   ylab(NULL) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(),
#     strip.text = element_text(size = 12, margin = margin(t = 5, b = 5)),  # Increase label size
#     strip.background = element_blank(),
#     strip.placement = "outside",
#     axis.text.x = element_text(vjust = 0, size = 10),  # Increase X-axis label size
#     axis.text.y = element_blank(),
#     legend.text = element_text(size = 14),  # Increase legend text size
#     legend.title = element_text(size = 14)  # Increase legend title size
#   )
# 
# # ggsave("E:/SDSU_GEOG/Thesis/Data/Signatures/figures_final/connected_sig_distributions.png", width = 12, height = 6, dpi = 300,bg = "white")
# 

