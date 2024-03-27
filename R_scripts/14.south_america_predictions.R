##############################################
# Plotting maps
##############################################

# south_america_ext <- rnaturalearth::ne_countries(scale = "medium",
#                                        returnclass = "sf") %>%
#   dplyr::filter(name %in% c("Brazil", "Argentina", "Uruguay", "Paraguay",
#                             "Venezuela", "Ecuador", "Colombia"))

south_america_ext <- rnaturalearth::ne_countries(scale = "medium",
                                                 returnclass = "sf") %>%
  dplyr::filter(continent == "South America")

south_america_ext <- st_set_crs(south_america_ext, 4326)

#######################################################
# ENSEMBLED MAP
#######################################################

south_america_ensembled_2010_raster  = raster::mask(world_ensembled_2010_raster, south_america_ext )

crs(south_america_ensembled_2010_raster ) = "EPSG:4326"
crs(south_america_ensembled_2010_raster ) == crs(south_america_ext)

south_america_ensembled_2010 = ggplot() +
  tidyterra::geom_spatraster(data = south_america_ensembled_2010_raster) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Add country borders
  geom_sf(data = south_america_ext, fill = NA, color = "black", size = 0.2) +
  
  coord_sf(
    xlim = c(-85, -30),
    ylim = c(-60, 15),
    crs = 4326,
    expand = FALSE
  ) +
  
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
  ) 

south_america_ensembled_2010

ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_ensembled_SouthAmerica.svg", sep = ""),
       south_america_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_ensembled_SouthAmerica.png", sep = ""),
       south_america_ensembled_2010, dpi = 400, height = 8, width = 8)

############################################
# BINARY MAP
############################################

south_america_binary_2010_raster  = raster::mask(world_binary_2010_raster, south_america_ext )

crs(south_america_binary_2010_raster ) = "EPSG:4326"

south_america_binary_2010 = ggplot() +
  tidyterra::geom_spatraster(data = south_america_binary_2010_raster, aes(fill = mean)) +
  
  scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.5,
    #breaks = seq(0, 1, 0.5)
  ) +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Add country borders
  geom_sf(data = south_america_ext, fill = NA, color = "black", size = 0.2) +
  
  coord_sf(
    xlim = c(-85, -30),
    ylim = c(-60, 15),
    crs = 4326,
    expand = FALSE
  ) +
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
  ) 

south_america_binary_2010 

ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_binary_SouthAmerica.svg", sep = ""),
       south_america_binary_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_binary_SouthAmerica.png", sep = ""),
       south_america_binary_2010, dpi = 400, height = 8, width = 8)

#######################################################
# south_america MODEL VALIDATION
#######################################################

head(species_braz)
pts = terra::vect(species_braz[,1:2])
crs(pts) = "EPSG:4326"

values_at_points <- terra::extract(x = south_america_binary_2010_raster, 
                                   y = pts, 
                                   xy = TRUE,
                                   na.rm = TRUE)

values_at_points = na.omit(values_at_points)

south_america_binary_2010 +
  geom_point(data = values_at_points, aes(x = x, y = y, colour = mean), size = 1, shape = 19 ) +
  scale_color_gradient(low = "red", high = "black") 

south_america_ensembled_2010 +
  geom_point(data = values_at_points, aes(x = x, y = y, colour = mean), size = 1, shape = 19 ) +
  scale_color_gradient(low = "red", high = "black") +
  guides(color = "none")


# Extract MaxEnt suitability scores at each GPS point
south_america_clim_pred_gps <- 
  terra::extract(
    x = south_america_ensembled_2010_raster,
    y = pts,
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
head(south_america_clim_pred_gps)

##############################################################
# Extract MaxEnt scores at background points 
##############################################################

# Coerce GPS records into SPDF
south_america_recordsSpatialInv <- sp::SpatialPointsDataFrame(
  coords = cbind(species_braz$lon, species_braz$lat),
  data = species_braz,
  proj4string = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  )
)

crs(pts) == crs(south_america_ensembled_2010_raster)

kg_sp <- as(kg_layer, "Spatial")

# Select KG ecoregions in which there is at least one GPS record
south_america_ecoContainInv <- kg_sp[south_america_recordsSpatialInv, ]

# Plot regions containing at least one record
# sp::plot(south_america_ecoContainInv)
# sp::plot(kg_layer,
#          add = TRUE,
#          col = 'gray70')
# Fill the ecoregions with a khaki colour if they contain at least 1 GPS point
# sp::plot(south_america_ecoContainInv,
#          add = TRUE,
#          col = 'khaki')
# Overlay GPS points
# points(species_south_america$lon,
#        species_south_america$lat,
#        pch = 21,
#        bg = 'mediumseagreen',
#        cex = 1)

south_america_ecoContainInv <- sf::st_as_sf(south_america_ecoContainInv)

crs(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds) = "EPSG:4326"
south_america_ecoContainInv = sf::st_set_crs(south_america_ecoContainInv, "EPSG:4326")
crs(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds) ==  crs(south_america_ecoContainInv)


south_america_bg_area_inv <- terra::mask(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds, 
                               south_america_ecoContainInv) 

set.seed(2023)

south_america_bg_points_inv <- terra::spatSample(
  x = south_america_bg_area_inv,        # Raster of background area to sample points from 
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
south_america_bgptsInv <- terra::vect(south_america_bg_points_inv)

# Set CRS for GPS coords 
crs(south_america_bgptsInv) <- "EPSG:4326"

# Extract MaxEnt suitability scores at each background GPS point
south_america_bg_clim_predInv <- 
  terra::extract(
    x = south_america_ensembled_2010_raster,
    y = south_america_bgptsInv,
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
head(south_america_bg_clim_predInv)

##############################################################
# Combine background and GPS points for occurrences  
##############################################################

# Combine dataframes 
south_america_clim_data <- 
  dplyr::bind_rows(
    south_america_clim_pred_gps,
    south_america_bg_clim_predInv
  )

##############################################################
# Fit statistical model  
##############################################################

# Run a simple binomial GLM
# Does the MaxEnt suitability score significantly affect the presence of the psyllid?
# This compares the suitability scores at the locations where the psyillid was recorded, to
# background points where it shouldn't occur

south_america_mod1 <- glm(
  # Response variable
  pres ~ 
    # Fixed effects 
    suit_score, 
  data = south_america_clim_data,
  family = binomial(link = "logit")
)
summary(south_america_mod1)

# Check model diagnostics 
DHARMa::simulateResiduals(fittedModel = south_america_mod1, plot = TRUE)

# Test parameter significance 
car::Anova(
  south_america_mod1, 
  test = "LR",
  type = "II"
)


##############################################################
# Plot statistical model  
##############################################################

# Extract model predictions 
south_america_preds <- ggeffects::ggpredict(
  south_america_mod1, 
  terms = c("suit_score [0:1 by = 0.01]")) %>%
  as.data.frame() %>%
  dplyr::rename(
    suit_score = x
  )
head(south_america_preds)

clim_data_presences = dplyr::filter(south_america_clim_data, pres == 1)
clim_data_absences = dplyr::filter(south_america_clim_data, pres == 0)

# Plot model predictions 
south_america_preds %>%
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
  south_america_mod1, 
  south_america_clim_data, 
  type = "response"
) 
head(predicted)

# Find optimal cutoff probability to use to maximize accuracy
optimal <- InformationValue::optimalCutoff(
  south_america_clim_data$pres, 
  #optimiseFor = "Ones",
  predicted)[1]
optimal

# Create confusion matrix
caret::confusionMatrix(
  data = as.factor(as.numeric(predicted>0.5)),
  reference = as.factor(south_america_clim_data$pres),
  positive = "1"
)

# Calculate sensitivity
# - Percentage of true positives 
InformationValue::sensitivity(
  actuals = as.factor(south_america_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)

# Calculate specificity
# - Percentage of true negatives
InformationValue::specificity(
  actuals = as.factor(south_america_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)

# Calculate total misclassification error rate
InformationValue::misClassError(
  actuals = as.factor(south_america_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)