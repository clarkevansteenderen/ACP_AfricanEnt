
citrus_sa = sf::st_read("citrus_growing_areas_sa")
geo_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
citrus_sa <- sf::st_transform (citrus_sa, geo_proj)

sa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                      returnclass = "sf") %>%
  # dplyr::filter(name == c("South Africa", "Lesotho", "Swaziland"))
  dplyr::filter(name %in% c("South Africa", "Lesotho", "Swaziland"))

sa_ext = st_set_crs(sa_ext, 4326)


class(citrus_sa)

current_sa_preds = terra::rast("G:/My Drive/ACP African Ent/acp_manuscript1/models_full_range_trained/all19_reduced_r2_VIF/africa_results_LQH6/diaphorina_citri/2010_ensembled.grd")
terra::crs(current_sa_preds) <- "EPSG:4326"
current_sa_preds_mask = raster::mask(current_sa_preds, sa_ext)

sp::plot(citrus_sa,  col = 'gray70')



# Load provincial borders data
provincial_borders <- ne_states(country = "South Africa", returnclass = "sf")



sa_preds_citrus_overlay = ggplot() +
  tidyterra::geom_spatraster(data = current_sa_preds_mask) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  
  # Add country borders
  geom_sf(data = sa_ext, fill = NA, color = "black", size = 0.2) +
  # Plot provincial borders
  geom_sf(data = provincial_borders, fill = NA, color = "grey35") +
  geom_sf(data = citrus_sa, colour = "black", fill = "black") +
  
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
  ) ;sa_preds_citrus_overlay

ggsave(filename = "sa_preds_citrus_overlay.png", 
       sa_preds_citrus_overlay, dpi = 400, height = 8, width = 8)
ggsave(filename = "sa_preds_citrus_overlay.svg", 
       sa_preds_citrus_overlay, dpi = 400, height = 8, width = 8)
