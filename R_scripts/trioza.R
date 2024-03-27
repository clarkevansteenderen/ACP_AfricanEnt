library(magrittr)
library(ggplot2)

sa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                      returnclass = "sf") %>%
              dplyr::filter(name %in% c("South Africa", "Lesotho", "Swaziland"))

sa_ext = sf::st_set_crs(sa_ext, 4326)

#test = terra::rast("2010_acp_raster.tif")
#terra::plot(test)

# Trioza map
trioza_input <- terra::rast("raster_pred_rsa_trioza_h4.tif")  

terra::ext(acp_map)
terra::ext(trioza_input)
# get the extents to match
trioza_input = terra::crop(trioza_input, acp_map)

terra::plot(trioza_input)

# Threshold map using a MaxEnt value of 0.778
# - I used the maxTSS from the MaxEnt html output (cloglog threshold)
trioza_t <- terra::ifel(trioza_input < 0.6, 0, 1)

terra::plot(trioza_t)

# Make plot
ggplot2::ggplot() +
  # Plot world boundary
  ggplot2::geom_sf(data = sa_ext, fill = NA) +
  # Plot raster layer
  tidyterra::geom_spatraster(data = trioza_t, aes(fill = maxent)) +
  
  tidyterra::scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.5,
    #breaks = seq(0, 1, 0.5)
  ) +
  theme(
    legend.position = "right"
  ) +
  coord_sf(
    xlim = c(15, 34),
    ylim = c(-21, -36),
    crs = 4326,
    expand = FALSE
  ) +
  theme_classic()

################
# ACP map
################

acp_map <- terra::rast("models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/2010_ensembled.grd")    # Load the ACP raster here
terra::plot(acp_map)

#raster::writeRaster(acp_map, "acp_raster.tif")

acp_map = raster::mask(acp_map, sa_ext)
terra::plot(acp_map)

# Threshold map using a MaxEnt value of 0.50              
# Need to change the threshold for ACP - maxTSS
acp_t <- terra::ifel(acp_map < 0.6, 0, 1)
terra::plot(acp_t)

# Make plot
ggplot2::ggplot() +
  # Plot world boundary
  ggplot2::geom_sf(data = sa_ext, fill = NA) +
  # Plot raster layer
  tidyterra::geom_spatraster(data = acp_t, aes(fill = median)) +
  
  tidyterra::scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.5,
    #breaks = seq(0, 1, 0.5)
  ) +
  theme(
    legend.position = "right"
  ) +
  coord_sf(
    xlim = c(15, 34),
    ylim = c(-21, -36),
    crs = 4326,
    expand = FALSE
  ) + 
  theme_classic()


trioza_alone = trioza_t & !acp_t
acp_alone = acp_t & !trioza_t
overlap = acp_t & trioza_t

terra::crs(overlap) = "EPSG:4326"
terra::crs(acp_alone) = "EPSG:4326"
terra::crs(trioza_alone) = "EPSG:4326"

overlap = terra::mask(overlap, sa_ext)
acp_alone = terra::mask(acp_alone, sa_ext)
trioza_alone = terra::mask(trioza_alone, sa_ext)

overlap_df = as.data.frame(overlap, xy = TRUE)
overlap_df$type = "overlap"

acp_alone_df = as.data.frame(acp_alone, xy = TRUE)
acp_alone_df$type = "acp_only"

trioza_alone_df = as.data.frame(trioza_alone, xy = TRUE) %>%
  dplyr::rename(
  median = maxent
)
trioza_alone_df$type = "trioza_only"

combined_df = rbind(overlap_df, acp_alone_df, trioza_alone_df)
head(combined_df)

# convert to raster
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
rast_comb <- sf::st_as_sf(x = combined_df,
                          coords = c("x", "y"),
                          crs = projcrs)

head(rast_comb)


# Make plot
overlaid_map = ggplot() +
  # Plot SA boundary
  geom_sf(data = sa_ext, fill = NA) +
  # Plot raster layer
  geom_sf(data = rast_comb, aes(colour = paste(median, type)), show.legend = FALSE) +
  scale_colour_manual(values = c("transparent", "transparent", "transparent", 
                                 "lightgrey", "black", "gold")) +

  coord_sf(
    xlim = c(15, 34),
    ylim = c(-21, -36),
    crs = 4326,
    expand = FALSE
  ) +
  theme_classic() +
  xlab("Longitude") +
  ylab("Latitude") +
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
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  ) 

ggsave(plot = overlaid_map, filename = "trioza_acp_overlap_0.6.png",
       dpi = 400, height = 8, width = 8)



# Plot using ggplot2
trioza_acp_overlap_map = ggplot() +
  
  geom_tile(data = overlap_df, aes(x = x, y = y, fill = median)) +
  
  scale_fill_manual(values = c("lightgrey","black"),
                    name = "Overlap?",  # Change legend title
                    labels = c("False", "True")) +  # Adjust colors as needed
  theme_classic() +
  coord_sf(
    xlim = c(15, 34),
    ylim = c(-21, -36),
    crs = 4326,
    expand = FALSE
  ) +
  xlab("Longitude") +
  ylab("Latitude") ;trioza_acp_overlap_map

ggsave(plot = trioza_acp_overlap_map, filename = "trioza_acp_overlap_0.7.png",
       dpi = 400, height = 8, width = 8)





overlap_plot = terra::plot(overlap, xlim = c(15, 34), ylim = c(-21, -36),
            col = c("lightgrey", "black"), 
            xlab = "Longitude",
            ylab = "Latitude")

acp_plot = terra::plot(acp_t, xlim = c(15, 34), ylim = c(-21, -36),
            col = c("lightgrey", "forestgreen"), 
            xlab = "Longitude",
            ylab = "Latitude")

te_plot = terra::plot(trioza_t, xlim = c(15, 34), ylim = c(-21, -36),
                       col = c("lightgrey", "royalblue"), 
                       xlab = "Longitude",
                       ylab = "Latitude")

terra::plot(acp_alone, xlim = c(15, 34), ylim = c(-21, -36),
                      col = c("lightgrey", "orange"), 
                      xlab = "Longitude",
                      ylab = "Latitude")

terra::plot(trioza_alone, xlim = c(15, 34), ylim = c(-21, -36),
            col = c("lightgrey", "purple"), 
            xlab = "Longitude",
            ylab = "Latitude")

png("overlay_plot.png", width = 10, height = 10, units = "cm", res = 400)

plot.new()
terra::plot(acp_alone, xlim = c(15, 34), ylim = c(-21, -36),
            col = c("lightgrey", "darkorange"), 
            xlab = "Longitude",
            ylab = "Latitude",
            legend = FALSE)

terra::plot(trioza_alone, add = TRUE,
            col = c("transparent", "purple"),
            legend = FALSE
            )

terra::plot(overlap, add = TRUE,
            col = c("transparent", "black"),
            legend = FALSE
)

# Save the plot
dev.off()




overlap_map = ggplot() +
  tidyterra::geom_spatraster(data = overlap, aes(fill = median)) +
  coord_sf(
    xlim = c(15, 34),
    ylim = c(-21, -36),
    crs = 4326,
    expand = FALSE
  ) + 
  scale_fill_manual(values = c("lightgrey", "black")) +
  theme_classic() ;overlap_map



terra::plot(acp_alone, xlim = c(15, 34), ylim = c(-21, -36),
            col = c("lightgrey", "black"))

terra::plot(trioza_t, xlim = c(15, 34), ylim = c(-21, -36),
            col = c("lightgrey", "black"))

terra::plot(acp_t, xlim = c(15, 34), ylim = c(-21, -36),
            col = c("lightgrey", "black"))


terra::plot(acp_t)

terra::plot(trioza_alone)
terra::plot(acp_alone)
terra::plot(overlap)

df_combo = rbind(df_tc, df_acp)

head(df_combo)



# Convert back to raster
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
rast_comb <- sf::st_as_sf(x = df_comb,
                          coords = c("x", "y"),
                          crs = projcrs)
terra::plot(rast_comb)
rast_comb
