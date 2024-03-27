# WTNR = world, trained on native range
# WTER = world, trained on entire range

WTNR_path = "models_native_range_trained/all19_reduced_r2_VIF/world_results_LQH1/diaphorina_citri/2010_ensembled.grd"
WTNR_ensembled_2010_raster  = terra::rast(WTNR_path)
terra::crs(WTNR_ensembled_2010_raster) <- "EPSG:4326"

WTER_path = "models_full_range_trained/all19_reduced_r2_VIF/world_results_LQH6/diaphorina_citri/2010_ensembled.grd"
WTER_ensembled_2010_raster  = terra::rast(WTER_path)
terra::crs(WTER_ensembled_2010_raster) <- "EPSG:4326"

world_diff = WTNR_ensembled_2010_raster - WTER_ensembled_2010_raster

ggplot() +
  tidyterra::geom_spatraster(data = WTNR_ensembled_2010_raster ) +
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
  ggtitle("2010, native range trained, LQH6, bio1, 7, 12, 14")


ggplot() +
  tidyterra::geom_spatraster(data = WTER_ensembled_2010_raster ) +
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
  ggtitle("2010, full range trained, LQH6, bio1, 7, 12, 19")



ggplot() +
  tidyterra::geom_spatraster(data = world_diff ) +
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
  # add country borders
  geom_sf(data = world_ext, fill = NA, color = "black") +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  ggtitle("2010, difference")




wdiff_rtp <- raster::rasterToPoints(raster::brick(world_diff) )
wdiff <- base::data.frame(wdiff_rtp)
head(wdiff)
colnames(wdiff) <- c("Longitude", "Latitude", "MaxEnt_diff")
max(wdiff$MaxEnt_diff)
min(wdiff$MaxEnt_diff)

wdiff = dplyr::filter(wdiff, MaxEnt_diff < -0.5 | MaxEnt_diff > 0.5)
head(wdiff)

world_ensembled_2010_thresholded = ggplot() +
  geom_tile(data = wdiff, aes(x = Longitude, y = Latitude), fill = "firebrick" ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # scale_fill_manual(
  #   values = c("gray90", "gray30"),
  #   labels = c("Unsuitable", "Suitable")
  # ) +
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
  pad_y = unit(0.2, "in"),
  height = unit(1, "cm"), width = unit(1, "cm"),
  style = north_arrow_fancy_orienteering
) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) +
  # add country borders
  geom_sf(data = world_ext, fill = NA, color = "black") +
  ggtitle("Thresholded: MaxEnt > 50%, 2010, LQH6, bio1, 7, 12, 19")

world_ensembled_2010_thresholded
