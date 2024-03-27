#################################
# RUN AFRICA PREDICTIONS
#################################

time_periods <- c(2010, 2030, 2050, 2070)
scenarios <- c("RCP4.5", "RCP8.5")

#user_model_choice =  tcltk::tk_choose.dir(caption = "SELECT directory with MaxEnt model (e.g. LITERATURE/wang_set/default_maxent_model)")
user_model_choice = readline("Directory with MaxEnt model (e.g. LITERATURE/wang_set/default_maxent_model): ")
user_model_choice = user_model_choice

#results_output = tcltk::tk_choose.dir(caption = "SELECT directory to save results (e.g. LITERATURE/wang_set/Africa_results)")
results_output = readline("Directory to save results (e.g. LITERATURE/wang_set/Africa_results/): ")
results_output = results_output


# Define a list of directories where the future climate layers are currently stored 
# save as a list

predictdir <- list(
  c(
    here::here(user_dir_2030_RCP4.5),
    here::here(user_dir_2050_RCP4.5),
    here::here(user_dir_2070_RCP4.5)
  ),
  c(
    here::here(user_dir_2030_RCP8.5),
    here::here(user_dir_2050_RCP8.5),
    here::here(user_dir_2070_RCP8.5)
  )
) 

##############################################
# Run the MaxEnt projections
##############################################

message("Running the MaxEnt projection....")

megaSDM::MaxEntProj(
  # Directory containing output from megaSDM::MaxEntModel
  input = here::here(user_model_choice),
  output = here::here(results_output), # create this folder manually
  time_periods = time_periods,
  scenarios = scenarios,
  # Directory containing all the current time period environmental layers 
  # - Requires .grd and .gri files
  study_dir = here::here(user_dir_2010),
  predict_dirs = predictdir,
  aucval = NA, 
  ncores = 2
)

##############################################
# Create time maps 
##############################################

message("Creating time maps....")

megaSDM::createTimeMaps(
  result_dir = results_output,
  time_periods = time_periods,
  scenarios = scenarios,
  dispersal = FALSE,
  ncores = 2
)

##############################################
# Additional statistics 
##############################################

message("Writing additional statistics...")

# The `additionalStats` function generates statistics on species range sizes 
# and changes through the multiple time steps and different scenarios. 
# - It also creates several graphs to visualize these changes.
megaSDM::additionalStats(
  result_dir = results_output,
  time_periods = time_periods,
  scenarios = scenarios,
  dispersal = FALSE,
  ncores = 2
)

##############################################
# Dispersal
##############################################

# dispersal values in km/year
dispersal_vals <- 
  data.frame(
    Species = "diaphorina_citri", 
    Rate = c(50)
  ); dispersal_vals

directo = "acp_manuscript1/models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6"
time_periods = c(2010, 2030, 2050, 2070) # need at least two dates here for it to works
scenarios = c("RCP4.5", "RCP8.5")

megaSDM::dispersalRate(
  result_dir = directo,
  time_periods = time_periods,
  scenarios = scenarios,
  dispersaldata = dispersal_vals,
  ncores = 2
)


##############################################
# Plotting maps
##############################################

# ENSEMBLED MAPS

africa_ensembled_2010_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/2010_ensembled.grd"
africa_ensembled_2010_raster  = terra::rast(africa_ensembled_2010_raster_path )
terra::crs(africa_ensembled_2010_raster) <- "EPSG:4326"

africa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                          returnclass = "sf") %>%
  dplyr::filter(continent == "Africa")

africa_ext <- st_set_crs(africa_ext, 4326)

#plot(africa_ensembled_2010_raster>0.75)

africa_ensembled_2010_raster  = raster::mask(africa_ensembled_2010_raster, africa_ext )

#######################################################
# ENSEMBLED MAP: AFRICA
#######################################################

africa_ensembled_2010 = ggplot() +
  tidyterra::geom_spatraster(data = africa_ensembled_2010_raster ) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat), size = 0.7) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2010, LQH6, bio1, 7, 12, 19")

africa_ensembled_2010

ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_ensembled.svg", sep = ""), 
       africa_ensembled_2010, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_ensembled.png", sep = ""), 
       africa_ensembled_2010, dpi = 500, height = 8, width = 8)



##################################
# 2030 RCP4.5
##################################

africa_ensembled_2030_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/RCP4.5/2030_RCP4.5_ensembled.grd"
africa_ensembled_2030_raster  = terra::rast(africa_ensembled_2030_raster_path )
terra::crs(africa_ensembled_2030_raster) <- "EPSG:4326"

#plot(africa_ensembled_2010_raster>0.75)

africa_ensembled_2030_raster  = raster::mask(africa_ensembled_2030_raster, africa_ext )

#######################################################
# ENSEMBLED MAP: AFRICA
#######################################################

africa_ensembled_2030 = ggplot() +
  tidyterra::geom_spatraster(data = africa_ensembled_2030_raster ) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  # geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2030 RCP4.5, LQH6, bio1, 7, 12, 19")

africa_ensembled_2030

ggsave(filename = paste(dirname(africa_ensembled_2030_raster_path), "/", "2030_ensembled.svg", sep = ""), 
       africa_ensembled_2030, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2030_raster_path), "/", "2030_ensembled.png", sep = ""), 
       africa_ensembled_2030, dpi = 500, height = 8, width = 8)


##################################
# 2050 RCP4.5
##################################

africa_ensembled_2050_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/RCP4.5/2050_RCP4.5_ensembled.grd"
africa_ensembled_2050_raster  = terra::rast(africa_ensembled_2050_raster_path )
terra::crs(africa_ensembled_2050_raster) <- "EPSG:4326"

#plot(africa_ensembled_2010_raster>0.75)

africa_ensembled_2050_raster  = raster::mask(africa_ensembled_2050_raster, africa_ext )

#######################################################
# ENSEMBLED MAP: AFRICA
#######################################################

africa_ensembled_2050 = ggplot() +
  tidyterra::geom_spatraster(data = africa_ensembled_2050_raster ) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  # geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2050 RCP4.5, LQH6, bio1, 7, 12, 19")

africa_ensembled_2050

ggsave(filename = paste(dirname(africa_ensembled_2050_raster_path), "/", "2050_ensembled.svg", sep = ""), 
       africa_ensembled_2050, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2050_raster_path), "/", "2050_ensembled.png", sep = ""), 
       africa_ensembled_2050, dpi = 500, height = 8, width = 8)



##################################
# 2070 RCP4.5
##################################

africa_ensembled_2070_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/RCP4.5/2070_RCP4.5_ensembled.grd"
africa_ensembled_2070_raster  = terra::rast(africa_ensembled_2070_raster_path )
terra::crs(africa_ensembled_2070_raster) <- "EPSG:4326"

#plot(africa_ensembled_2010_raster>0.75)

africa_ensembled_2070_raster  = raster::mask(africa_ensembled_2070_raster, africa_ext )

#######################################################
# BINARY MAP: AFRICA
#######################################################

africa_ensembled_2070 = ggplot() +
  tidyterra::geom_spatraster(data = africa_ensembled_2070_raster ) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  # geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2070 RCP4.5, LQH6, bio1, 7, 12, 19")

africa_ensembled_2070

ggsave(filename = paste(dirname(africa_ensembled_2070_raster_path), "/", "2070_ensembled.svg", sep = ""), 
       africa_ensembled_2070, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2070_raster_path), "/", "2070_ensembled.png", sep = ""), 
       africa_ensembled_2070, dpi = 500, height = 8, width = 8)


ensembled_maps = gridExtra::grid.arrange(africa_ensembled_2010, africa_ensembled_2030,
                        africa_ensembled_2050, africa_ensembled_2070)

ggsave(filename = paste(dirname(africa_ensembled_2070_raster_path), "/", "ensembled_maps.png", sep = ""), 
       ensembled_maps, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2070_raster_path), "/", "ensembled_maps.svg", sep = ""), 
       ensembled_maps, dpi = 500, height = 8, width = 8)

##########
#SA
##########

sa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                      returnclass = "sf") %>%
  # dplyr::filter(name == c("South Africa", "Lesotho", "Swaziland"))
  dplyr::filter(name %in% c("South Africa", "Lesotho", "Swaziland"))

sa_ext = st_set_crs(sa_ext, 4326)

south_africa_ensembled_2010_raster = raster::mask(africa_ensembled_2010_raster, sa_ext)

# Load provincial borders data
provincial_borders <- ne_states(country = "South Africa", returnclass = "sf")

#########

# plot

south_africa_ensembled_2010 = ggplot() +
  tidyterra::geom_spatraster(data = south_africa_ensembled_2010_raster) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  
  # Add country borders
  geom_sf(data = sa_ext, fill = NA, color = "black", size = 0.2) +
  # Plot provincial borders
  geom_sf(data = provincial_borders, fill = NA, color = "black") +
  
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Crops map to just the geographic extent of SA
  coord_sf(
    xlim = c(15, 34),
    ylim = c(-21, -36),
    crs = 4326,
    expand = FALSE
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "br",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  ggtitle("2010")

south_africa_ensembled_2010

ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_ensembled_SA.svg", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_ensembled_SA.png", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)


##########################
# BINARY MAPS

# 2010

africa_binary_2010_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/2010_binary.grd"
africa_binary_2010_raster  = terra::rast(africa_binary_2010_raster_path )
terra::crs(africa_binary_2010_raster) <- "EPSG:4326"

#plot(africa_ensembled_2010_raster>0.75)

africa_binary_2010_raster  = raster::mask(africa_binary_2010_raster, africa_ext )

#######################################################
# BINARY MAP: AFRICA
#######################################################

africa_binary_2010 = ggplot() +
  tidyterra::geom_spatraster(data = africa_binary_2010_raster ) +
  scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.65
  ) +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2010, LQH6, bio1, 7, 12, 19")

africa_binary_2010

ggsave(filename = paste(dirname(africa_binary_2010_raster_path), "/", "2010_binary.svg", sep = ""), 
       africa_binary_2010, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_binary_2010_raster_path), "/", "2010_binary.png", sep = ""), 
       africa_binary_2010, dpi = 500, height = 8, width = 8)

##################################
# 2030 RCP4.5
##################################

africa_binary_2030_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/RCP4.5/2030_RCP4.5_binary.grd"
africa_binary_2030_raster  = terra::rast(africa_binary_2030_raster_path )
terra::crs(africa_binary_2030_raster) <- "EPSG:4326"

#plot(africa_ensembled_2010_raster>0.75)

africa_binary_2030_raster  = raster::mask(africa_binary_2030_raster, africa_ext )

#######################################################
# BINARY MAP: AFRICA
#######################################################

africa_binary_2030 = ggplot() +
  tidyterra::geom_spatraster(data = africa_binary_2030_raster ) +
  scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.65
  ) +
 # geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2030 RCP4.5, LQH6, bio1, 7, 12, 19")

africa_binary_2030

ggsave(filename = paste(dirname(africa_binary_2030_raster_path), "/", "2030_binary.svg", sep = ""), 
       africa_binary_2030, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_binary_2030_raster_path), "/", "2030_binary.png", sep = ""), 
       africa_binary_2030, dpi = 500, height = 8, width = 8)


##################################
# 2050 RCP4.5
##################################

africa_binary_2050_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/RCP4.5/2050_RCP4.5_binary.grd"
africa_binary_2050_raster  = terra::rast(africa_binary_2050_raster_path )
terra::crs(africa_binary_2050_raster) <- "EPSG:4326"

#plot(africa_ensembled_2010_raster>0.75)

africa_binary_2050_raster  = raster::mask(africa_binary_2050_raster, africa_ext )

#######################################################
# BINARY MAP: AFRICA
#######################################################

africa_binary_2050 = ggplot() +
  tidyterra::geom_spatraster(data = africa_binary_2050_raster ) +
  scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.65
  ) +
  # geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2050 RCP4.5, LQH6, bio1, 7, 12, 19")

africa_binary_2050

ggsave(filename = paste(dirname(africa_binary_2050_raster_path), "/", "2050_binary.svg", sep = ""), 
       africa_binary_2050, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_binary_2050_raster_path), "/", "2050_binary.png", sep = ""), 
       africa_binary_2050, dpi = 500, height = 8, width = 8)



##################################
# 2070 RCP4.5
##################################

africa_binary_2070_raster_path = "models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/RCP4.5/2070_RCP4.5_binary.grd"
africa_binary_2070_raster  = terra::rast(africa_binary_2070_raster_path )
terra::crs(africa_binary_2070_raster) <- "EPSG:4326"

#plot(africa_ensembled_2010_raster>0.75)

africa_binary_2070_raster  = raster::mask(africa_binary_2070_raster, africa_ext )

#######################################################
# BINARY MAP: AFRICA
#######################################################

africa_binary_2070 = ggplot() +
  tidyterra::geom_spatraster(data = africa_binary_2070_raster ) +
  scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.65
  ) +
  # geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.1) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2070 RCP4.5, LQH6, bio1, 7, 12, 19")

africa_binary_2070

ggsave(filename = paste(dirname(africa_binary_2070_raster_path), "/", "2070_binary.svg", sep = ""), 
       africa_binary_2070, dpi = 500, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_binary_2070_raster_path), "/", "2070_binary.png", sep = ""), 
       africa_binary_2070, dpi = 500, height = 8, width = 8)


gridExtra::grid.arrange(africa_binary_2010, africa_binary_2030,
                        africa_binary_2050, africa_binary_2070)

##########
#SA
##########

sa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                      returnclass = "sf") %>%
  # dplyr::filter(name == c("South Africa", "Lesotho", "Swaziland"))
  dplyr::filter(name %in% c("South Africa", "Lesotho", "Swaziland"))

sa_ext = st_set_crs(sa_ext, 4326)

south_africa_ensembled_2010_raster = raster::mask(africa_ensembled_2010_raster, sa_ext)

# Load provincial borders data
provincial_borders <- ne_states(country = "South Africa", returnclass = "sf")

#########

# plot

south_africa_ensembled_2010 = ggplot() +
  tidyterra::geom_spatraster(data = south_africa_ensembled_2010_raster) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  
  # Add country borders
  geom_sf(data = sa_ext, fill = NA, color = "black", size = 0.2) +
  # Plot provincial borders
  geom_sf(data = provincial_borders, fill = NA, color = "black") +
  
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Crops map to just the geographic extent of SA
  coord_sf(
    xlim = c(15, 34),
    ylim = c(-21, -36),
    crs = 4326,
    expand = FALSE
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "br",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  ggtitle("2010")

south_africa_ensembled_2010

ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_ensembled_SA.svg", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_ensembled_SA.png", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)

##############################################
# ENSEMBLED MAP, THRESHOLDED TO PROB > 75%
##############################################

rcp = africa_ensembled_2010_raster
plot(rcp)

rcp <- raster::rasterToPoints(raster::brick(africa_ensembled_2010_raster) )
rcpdf <- base::data.frame(rcp)
head(rcpdf)
colnames(rcpdf) <- c("Longitude", "Latitude", "MaxEnt_score")
max(rcpdf$MaxEnt_score)
min(rcpdf$MaxEnt_score)

rcpdf = dplyr::filter(rcpdf, rcpdf$MaxEnt_score >= 0.75)
head(rcpdf)
max(rcpdf$MaxEnt_score)
min(rcpdf$MaxEnt_score)

africa_ensembled_2010_thresholded = ggplot() +
  # add country borders
  geom_sf(data = africa_ext, fill = "grey90", color = "black") +
  geom_tile(data = rcpdf, aes(x = Longitude, y = Latitude), fill = "grey20" ) +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat), size = 0.7, col = "red") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.2, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("Thresholded: MaxEnt > 75%, 2010, LQH6, bio1, 7, 12, 19")

africa_ensembled_2010_thresholded

ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_75percent_thresh.svg", sep = ""), 
       africa_ensembled_2010_thresholded, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2010_75percent_thresh.png", sep = ""), 
       africa_ensembled_2010_thresholded, dpi = 400, height = 8, width = 8)



#############################
# time maps

t70_t10 = africa_binary_2070_raster - africa_binary_2010_raster


t70_t10_rcp <- raster::rasterToPoints(raster::brick(t70_t10) )
t70_t10_rcpdf <- base::data.frame(t70_t10_rcp)
head(t70_t10_rcpdf)
colnames(t70_t10_rcpdf) <- c("Longitude", "Latitude", "MaxEnt_score")

ggplot() +
  
  geom_tile(data = t70_t10_rcpdf, aes(x = Longitude, y = Latitude, fill = factor(MaxEnt_score))) +
  scale_fill_manual(values = c("-1" = "red", "0" = "grey", "1" = "blue")) +
  # add country borders
  geom_sf(data = africa_ext, fill = "grey90", color = "black") +
  # # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.2, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("Thresholded: MaxEnt > 75%, 2010, LQH6, bio1, 7, 12, 19")

