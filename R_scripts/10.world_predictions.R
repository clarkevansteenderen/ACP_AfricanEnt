#################################
# RUN WORLD PREDICTIONS
#################################

time_periods <- c(2010, 2050, 2070)
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
    here::here(user_dir_2050_RCP4.5),
    here::here(user_dir_2070_RCP4.5)
  ),
  c(
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
# Plotting maps
##############################################



world_ensembled_2010_raster_path = "models_native_range_trained/all19_reduced_r2_VIF/world_predictions_LQH1/diaphorina_citri/2010_ensembled.grd"
world_ensembled_2010_raster  = terra::rast(world_ensembled_2010_raster_path )
terra::crs(world_ensembled_2010_raster) <- "EPSG:4326"

africa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                          returnclass = "sf") %>%
  dplyr::filter(continent == "Africa")

africa_ext <- st_set_crs(africa_ext, 4326)


#######################################################
# ENSEMBLED MAP: WORLD
#######################################################

world_ensembled_2010 = ggplot() +
  tidyterra::geom_spatraster(data = world_ensembled_2010_raster ) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # # Add scale bar to bottom-right of map
  # ggspatial::annotation_scale(
  #   location = "bl",          # 'bl' = bottom left
  #   style = "ticks",
  #   width_hint = 0.2,
  #   pad_x = unit(0.2, "in"),
  #   pad_y = unit(0.15, "in")
  # ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.1, "in"),
    height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  ggtitle("2010, LQH1, bio1, 7, 12, 14")

world_ensembled_2010

ggsave(filename = paste(dirname(world_ensembled_2010_raster_path), "/", "2010_ensembled.svg", sep = ""), 
       world_ensembled_2010, dpi = 500, height = 8, width = 8)

#######################################################
# ENSEMBLED MAP: AFRICA
#######################################################

#message("Select the ensembled .grd file to plot in the popup window")
#africa_ensembled_2010_raster_path = file.choose()

africa_ensembled_2010_raster  = terra::rast(world_ensembled_2010_raster_path )
africa_ensembled_2010_raster  = raster::mask(africa_ensembled_2010_raster, africa_ext )

crs(africa_ensembled_2010_raster ) = "EPSG:4326"

africa_ensembled_2010 = ggplot() +
  tidyterra::geom_spatraster(data = africa_ensembled_2010_raster) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  
  # Add country borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.2) +
  
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2,
    pad_x = unit(0.25, "in"),
    pad_y = unit(0.15, "in")
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  ggtitle("2010")

africa_ensembled_2010

ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2070_RCP8.5_ensembled.svg", sep = ""), 
       africa_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2070_RCP8.5_ensembled.png", sep = ""), 
       africa_ensembled_2010, dpi = 400, height = 8, width = 8)

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
  ggtitle("2070, RCP8.5")

south_africa_ensembled_2010

south_africa_ensembled_2010 = south_africa_ensembled_2010 +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat), size = 0.8, shape = 19)

ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2070_RCP8.5_ensembled_SA.svg", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_ensembled_2010_raster_path), "/", "2070_RCP8.5_ensembled_SA.png", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)


############################################
# BINARY MAP
############################################

message("Select the binary .grd file to plot in the popup window")
africa_binary_2010_raster_path = file.choose()

africa_binary_2010_raster  = terra::rast(africa_binary_2010_raster_path )
africa_binary_2010_raster  = raster::mask(africa_binary_2010_raster, africa_ext )

crs(africa_binary_2010_raster ) = "EPSG:4326"

africa_binary_2010 = ggplot() +
  tidyterra::geom_spatraster(data = africa_binary_2010_raster, aes(fill = mean)) +
  
  scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.5,
    #breaks = seq(0, 1, 0.5)
  ) +
  # Add country borders
  geom_sf(data = africa_ext, fill = NA, color = "black", size = 0.2) +
  
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Create title for the legend
  theme(legend.position = "none") +
  # Add scale bar to bottom-right of map
  ggspatial::annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2
  ) +
  # Add north arrow
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.175, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  coord_sf(ylim = c(-35, 40))

ggsave(filename = paste(dirname(africa_binary_2010_raster_path), "/", "2010_binary.svg", sep = ""), 
       africa_binary_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(africa_binary_2010_raster_path), "/", "2010_binary.png", sep = ""), 
       africa_binary_2010, dpi = 400, height = 8, width = 8)
