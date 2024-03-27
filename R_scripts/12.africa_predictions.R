#######################################################
# ENSEMBLED MAP: AFRICA
#######################################################

world_ensembled_2010_raster_path = "models_native_range_trained/all19_reduced_r2_VIF/world_results_LQH1/diaphorina_citri/2010_ensembled.grd"
world_ensembled_2010_raster  = terra::rast(world_ensembled_2010_raster_path )
terra::crs(world_ensembled_2010_raster) <- "EPSG:4326"

africa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                          returnclass = "sf") %>%
  dplyr::filter(continent == "Africa")

africa_ext <- st_set_crs(africa_ext, 4326)

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
  geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
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

ggsave(filename = paste(dirname(world_ensembled_2010_raster_path), "/", "2010_ensembled_africa.svg", sep = ""), 
       africa_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(world_ensembled_2010_raster_path), "/", "2010_ensembled_africa.png", sep = ""), 
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
  ggtitle("2010")

south_africa_ensembled_2010

ggsave(filename = paste(dirname(world_ensembled_2010_raster_path), "/", "2010_ensembled_SA.svg", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(world_ensembled_2010_raster_path), "/", "2010_ensembled_SA.png", sep = ""), 
       south_africa_ensembled_2010, dpi = 400, height = 8, width = 8)


############################################
# BINARY MAP
############################################

world_binary_2010_raster_path = "models_native_range_trained/all19_reduced_r2_VIF/world_results_LQH1/diaphorina_citri/2010_binary.grd"

world_binary_2010_raster  = terra::rast(world_binary_2010_raster_path )
africa_binary_2010_raster  = raster::mask(world_binary_2010_raster, africa_ext )

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
  geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
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
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55))

africa_binary_2010

ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_binary_Africa.svg", sep = ""), 
       africa_binary_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_binary_Africa.png", sep = ""), 
       africa_binary_2010, dpi = 400, height = 8, width = 8)



###########################################################
# RUN SOME STATS ON THE KNOWN OCCURRENCE GPS POINTS
###########################################################

#######################################################
# AFRICA MODEL VALIDATION
#######################################################

head(species_afr)
afr_pts = terra::vect(species_afr)
crs(afr_pts) = "EPSG:4326"

africa_values_at_points <- terra::extract(x = africa_binary_2010_raster, 
                                   y = afr_pts, 
                                   xy = TRUE,
                                   na.rm = TRUE)

africa_values_at_points = na.omit(africa_values_at_points)

# find the number of 1's and 0's
length(africa_values_at_points$mean)
length(which(africa_values_at_points$mean == 1))
52/78*100
length(which(africa_values_at_points$mean == 0))
26/78*100


africa_binary_2010 +
  geom_point(data = africa_values_at_points, aes(x = x, y = y, colour = mean), size = 1, shape = 19 ) +
  scale_color_gradient(low = "red", high = "black") 

africa_ensembled_2010 +
  geom_point(data = africa_values_at_points, aes(x = x, y = y, colour = mean), size = 1, shape = 19 ) +
  scale_color_gradient(low = "red", high = "black") +
  guides(color = "none")


# Extract MaxEnt suitability scores at each GPS point
africa_clim_pred_gps <- 
  terra::extract(
    x = africa_ensembled_2010_raster,
    y = afr_pts,
    xy = TRUE,
    na.rm = TRUE
  ) %>%
  # Clean df
  dplyr::select(
    lat = y,
    lon = x,
    suit_score = 2 # assign the name of the second column ("median") to "suit_score"
  ) %>%
  tidyr::drop_na(suit_score) %>%
  dplyr::mutate(pres = 1)
head(africa_clim_pred_gps)

##############################################################
# Extract MaxEnt scores at background points in Africa range
##############################################################

# Coerce GPS records into SPDF
africa_recordsSpatialInv <- sp::SpatialPointsDataFrame(
  coords = cbind(species_afr$lon, species_afr$lat),
  data = species_afr,
  proj4string = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  )
)

crs(afr_pts) == crs(africa_ensembled_2010_raster)

kg_sp <- as(kg_layer, "Spatial")

# Select KG ecoregions in which there is at least one GPS record
africa_ecoContainInv <- kg_sp[africa_recordsSpatialInv, ]

# Plot regions containing at least one record
#sp::plot(africa_ecoContainInv)
# sp::plot(kg_layer, 
#          add = TRUE, 
#          col = 'gray70')
# Fill the ecoregions with a khaki colour if they contain at least 1 GPS point
# sp::plot(africa_ecoContainInv, 
#          add = TRUE, 
#          col = 'khaki')
# Overlay GPS points 
# points(species_afr$lon, 
#        species_afr$lat, 
#        pch = 21, 
#        bg = 'mediumseagreen', 
#        cex = 1)

africa_ecoContainInv <- sf::st_as_sf(africa_ecoContainInv)

crs(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds) = "EPSG:4326"
africa_ecoContainInv = sf::st_set_crs(africa_ecoContainInv, "EPSG:4326")
crs(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds) ==  crs(africa_ecoContainInv)


africa_bg_area_inv <- terra::mask(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds, 
                                  africa_ecoContainInv) 

set.seed(2023)

africa_bg_points_inv <- terra::spatSample(
  x = africa_bg_area_inv,        # Raster of background area to sample points from 
  size = 1000,        # How many background points do we want?
  method = "random",  # Random points
  replace = FALSE,    # Sample without replacement
  na.rm = TRUE,       # Remove background points that have NA climate data
  as.df = TRUE,       # Return background points as data.frame object
  xy = TRUE           # Return lat/lon values for each background point
  #cells = TRUE       # Return the cell numbers in which the background points fall
) %>%
  # Rename lon and lat columns to be consistent with GPS data for focal species 
  dplyr::rename(
    lon = x,
    lat = y
  )

# Convert GPS coords to a 'terra' SpatialVector 
africa_bgafr_ptsInv <- terra::vect(africa_bg_points_inv)

# Set CRS for GPS coords 
crs(africa_bgafr_ptsInv) <- "EPSG:4326"

# Extract MaxEnt suitability scores at each background GPS point
africa_bg_clim_predInv <- 
  terra::extract(
    x = africa_ensembled_2010_raster,
    y = africa_bgafr_ptsInv,
    xy = TRUE,
    na.rm = TRUE
  ) %>%
  # Clean df
  dplyr::select(
    lat = y,
    lon = x,
    suit_score = 2
  ) %>%
  tidyr::drop_na(suit_score) %>%
  dplyr::mutate(pres = 0)
head(africa_bg_clim_predInv)

##############################################################
# Combine background and GPS points for occurrences  
##############################################################

# Combine dataframes 
africa_clim_data <- 
  dplyr::bind_rows(
    africa_clim_pred_gps,
    africa_bg_clim_predInv
  )

##############################################################
# Fit statistical model  
##############################################################

# Run a simple binomial GLM
# Does the MaxEnt suitability score significantly affect the presence of the psyllid?
# This compares the suitability scores at the locations where the psyillid was recorded, to
# background points where it shouldn't occur

africa_mod1 <- glm(
  # Response variable
  pres ~ 
    # Fixed effects 
    suit_score, 
  data = africa_clim_data,
  family = binomial(link = "logit")
)
summary(africa_mod1)

# Check model diagnostics 
DHARMa::simulateResiduals(fittedModel = africa_mod1, plot = TRUE)

# Test parameter significance 
car::Anova(
  africa_mod1, 
  test = "LR",
  type = "II"
)


##############################################################
# Plot statistical model  
##############################################################

# Extract model predictions 
africa_preds <- ggeffects::ggpredict(
  africa_mod1, 
  terms = c("suit_score [0:1 by = 0.01]")) %>%
  as.data.frame() %>%
  dplyr::rename(
    suit_score = x
  )
head(africa_preds)

clim_data_presences = dplyr::filter(africa_clim_data, pres == 1)
clim_data_absences = dplyr::filter(africa_clim_data, pres == 0)

# Plot model predictions 
africa_preds %>%
  ggplot2::ggplot(data = ., 
                  aes(x = suit_score,
                      y = predicted)) +
  geom_rug(data = clim_data_presences, aes(x= suit_score, y = pres), sides = "t") + 
  geom_rug(data = clim_data_absences, aes(x= suit_score, y = pres), sides = "b") +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.1) +
  labs(
    x = "MaxEnt suitability score",
    y = "Probability of being recorded"
  )

##############################################################
# Calculate model accuracy metrics 
##############################################################

# Use model to predict probability of default
predicted <- predict(
  africa_mod1, 
  africa_clim_data, 
  type = "response"
) 
head(predicted)

# Find optimal cutoff probability to use to maximize accuracy
optimal <- InformationValue::optimalCutoff(
  africa_clim_data$pres, 
  #optimiseFor = "Ones",
  predicted)[1]
optimal

# Create confusion matrix
caret::confusionMatrix(
  data = as.factor(as.numeric(predicted>0.5)),
  reference = as.factor(africa_clim_data$pres),
  positive = "1"
)

# Calculate sensitivity
# - Percentage of true positives 
InformationValue::sensitivity(
  actuals = as.factor(africa_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)

# Calculate specificity
# - Percentage of true negatives
InformationValue::specificity(
  actuals = as.factor(africa_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)

# Calculate total misclassification error rate
InformationValue::misClassError(
  actuals = as.factor(africa_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)
