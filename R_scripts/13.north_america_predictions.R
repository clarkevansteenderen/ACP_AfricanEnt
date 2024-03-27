##############################################
# Plotting maps
##############################################

usa_ext <- rnaturalearth::ne_countries(scale = "medium",
                                       returnclass = "sf") %>%
  dplyr::filter(name %in% c("Mexico", "United States", "Belize", "Honduras", "El Salvador",
                            "Guatemala", "Cuba", "Puerto Rico", "Dominican Republic", "Haiti",
                            "Costa Rica", "Panama", 
                            "Jamaica", "The Bahamas", "Cayman Islands", "Anguilla", "Montserrat",
                            "Guadeloupe", "Dominica", "Martinique", "St Lucia", "Barbados", "Grenada",
                            "Trinidad and Tobago", "St Kitts and Nevis", "US Virgin Islands", 
                            "British Virgin Islands", "St Vincent and the Grenadines"))

usa_ext <- st_set_crs(usa_ext, 4326)

#######################################################
# ENSEMBLED MAP
#######################################################

usa_ensembled_2010_raster  = raster::mask(world_ensembled_2010_raster, usa_ext )

crs(usa_ensembled_2010_raster ) = "EPSG:4326"
crs(usa_ensembled_2010_raster ) == crs(usa_ext)

usa_ensembled_2010 = ggplot() +
  tidyterra::geom_spatraster(data = usa_ensembled_2010_raster) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Add country borders
  geom_sf(data = usa_ext, fill = NA, color = "black", size = 0.2) +
  
  coord_sf(
    xlim = c(-127, -55),
    ylim = c(50, 0),
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
  ) 

usa_ensembled_2010

ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_ensembled_USA.svg", sep = ""),
       usa_ensembled_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_ensembled_USA.png", sep = ""),
       usa_ensembled_2010, dpi = 400, height = 8, width = 8)

############################################
# BINARY MAP
############################################

usa_binary_2010_raster  = raster::mask(world_binary_2010_raster, usa_ext )

crs(usa_binary_2010_raster ) = "EPSG:4326"

usa_binary_2010 = ggplot() +
  tidyterra::geom_spatraster(data = usa_binary_2010_raster, aes(fill = mean)) +
  
  scale_fill_whitebox_b(
    palette = "gn_yl",
    direction = -1,
    limits = c(0, 1),
    alpha = 0.5,
    #breaks = seq(0, 1, 0.5)
  ) +
  geom_point(data = sp_data_thin, aes(x = lon, y = lat)) +
  # Add country borders
  geom_sf(data = usa_ext, fill = NA, color = "black", size = 0.2) +
  
  coord_sf(
    xlim = c(-127, -55),
    ylim = c(50, 0),
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
  ) 

usa_binary_2010 

ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_binary_USA.svg", sep = ""),
       usa_binary_2010, dpi = 400, height = 8, width = 8)
ggsave(filename = paste(dirname(world_binary_2010_raster_path), "/", "2010_binary_USA.png", sep = ""),
       usa_binary_2010, dpi = 400, height = 8, width = 8)

#######################################################
# USA MODEL VALIDATION
#######################################################

head(species_usa)
pts = terra::vect(species_usa[,1:2])
crs(pts) = "EPSG:4326"

usa_values_at_points <- terra::extract(x = usa_binary_2010_raster, 
                                   y = pts, 
                                   xy = TRUE,
                                   na.rm = TRUE)

usa_values_at_points = na.omit(usa_values_at_points)

# find the number of 1's and 0's
length(usa_values_at_points$mean)
length(which(usa_values_at_points$mean == 1))
87/126*100
length(which(usa_values_at_points$mean == 0))
39/126*100

usa_binary_2010 +
  geom_point(data = usa_values_at_points, aes(x = x, y = y, colour = mean), size = 1, shape = 19 ) +
  scale_color_gradient(low = "red", high = "black") 

usa_ensembled_2010 +
  geom_point(data = usa_values_at_points, aes(x = x, y = y, colour = mean), size = 1, shape = 19 ) +
  scale_color_gradient(low = "red", high = "black") +
  guides(color = "none")


# Extract MaxEnt suitability scores at each GPS point
usa_clim_pred_gps <- 
  terra::extract(
    x = usa_ensembled_2010_raster,
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
head(usa_clim_pred_gps)

##############################################################
# Extract MaxEnt scores at background points 
##############################################################

# Coerce GPS records into SPDF
usa_recordsSpatialInv <- sp::SpatialPointsDataFrame(
  coords = cbind(species_usa$lon, species_usa$lat),
  data = species_usa,
  proj4string = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  )
)

crs(pts) == crs(usa_ensembled_2010_raster)

kg_sp <- as(kg_layer, "Spatial")

# Select KG ecoregions in which there is at least one GPS record
usa_ecoContainInv <- kg_sp[usa_recordsSpatialInv, ]

# Plot regions containing at least one record
# sp::plot(usa_ecoContainInv)
# sp::plot(kg_layer,
#          add = TRUE,
#          col = 'gray70')
# Fill the ecoregions with a khaki colour if they contain at least 1 GPS point
# sp::plot(usa_ecoContainInv,
#          add = TRUE,
#          col = 'khaki')
# Overlay GPS points
# points(species_usa$lon,
#        species_usa$lat,
#        pch = 21,
#        bg = 'mediumseagreen',
#        cex = 1)

usa_ecoContainInv <- sf::st_as_sf(usa_ecoContainInv)

crs(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds) = "EPSG:4326"
usa_ecoContainInv = sf::st_set_crs(usa_ecoContainInv, "EPSG:4326")
crs(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds) ==  crs(usa_ecoContainInv)


usa_bg_area_inv <- terra::mask(predictor_spatrasters_entire$all_19_reduced_r2_VIF_preds, 
                               usa_ecoContainInv) 

set.seed(2023)

usa_bg_points_inv <- terra::spatSample(
  x = usa_bg_area_inv,        # Raster of background area to sample points from 
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
usa_bgptsInv <- terra::vect(usa_bg_points_inv)

# Set CRS for GPS coords 
crs(usa_bgptsInv) <- "EPSG:4326"

# Extract MaxEnt suitability scores at each background GPS point
usa_bg_clim_predInv <- 
  terra::extract(
    x = usa_ensembled_2010_raster,
    y = usa_bgptsInv,
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
head(usa_bg_clim_predInv)

##############################################################
# Combine background and GPS points for occurrences  
##############################################################

# Combine dataframes 
usa_clim_data <- 
  dplyr::bind_rows(
    usa_clim_pred_gps,
    usa_bg_clim_predInv
  )

##############################################################
# Fit statistical model  
##############################################################

# Run a simple binomial GLM
# Does the MaxEnt suitability score significantly affect the presence of the psyllid?
# This compares the suitability scores at the locations where the psyillid was recorded, to
# background points where it shouldn't occur

usa_mod1 <- glm(
  # Response variable
  pres ~ 
    # Fixed effects 
    suit_score, 
  data = usa_clim_data,
  family = binomial(link = "logit")
)
summary(usa_mod1)

# Check model diagnostics 
DHARMa::simulateResiduals(fittedModel = usa_mod1, plot = TRUE)

# Test parameter significance 
car::Anova(
  usa_mod1, 
  test = "LR",
  type = "II"
)


##############################################################
# Plot statistical model  
##############################################################

# Extract model predictions 
usa_preds <- ggeffects::ggpredict(
  usa_mod1, 
  terms = c("suit_score [0:1 by = 0.01]")) %>%
  as.data.frame() %>%
  dplyr::rename(
    suit_score = x
  )
head(usa_preds)

clim_data_presences = dplyr::filter(usa_clim_data, pres == 1)
clim_data_absences = dplyr::filter(usa_clim_data, pres == 0)

# Plot model predictions 
usa_preds %>%
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
  usa_mod1, 
  usa_clim_data, 
  type = "response"
) 
head(predicted)

# Find optimal cutoff probability to use to maximize accuracy
optimal <- InformationValue::optimalCutoff(
  usa_clim_data$pres, 
  #optimiseFor = "Ones",
  predicted)[1]
optimal

# Create confusion matrix
caret::confusionMatrix(
  data = as.factor(as.numeric(predicted>0.5)),
  reference = as.factor(usa_clim_data$pres),
  positive = "1"
)

# Calculate sensitivity
# - Percentage of true positives 
InformationValue::sensitivity(
  actuals = as.factor(usa_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)

# Calculate specificity
# - Percentage of true negatives
InformationValue::specificity(
  actuals = as.factor(usa_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)

# Calculate total misclassification error rate
InformationValue::misClassError(
  actuals = as.factor(usa_clim_data$pres),
  predicted = predicted, 
  threshold = optimal
)