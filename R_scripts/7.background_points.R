kg_layer = sf::st_read("data/shapefiles/koppen_geiger")

# Reproject KG-layer
geo_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
kg_layer <- sf::st_transform (kg_layer, geo_proj)

# Coerce focal taxon GPS records into SpatialPointsDataFrame (SPDF)
records_spatial <- sp::SpatialPointsDataFrame(
  coords = cbind(training_dataset$lon, training_dataset$lat),
  data = training_dataset,
  proj4string = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  )
)

# Select KG ecoregions in which there is at least one GPS record
kg_sp <- as(kg_layer, "Spatial")
kg_contain <- kg_sp[records_spatial, ]


# Plot regions containing at least one record
# sp::plot(kg_contain)
# 
# sp::plot(
#   kg_layer,
#   add = TRUE,
#   col = 'gray70'
# )

# Fill the KG zones with a khaki colour if they contain at least 1 GPS point
# sp::plot(
#   kg_contain,
#   add = TRUE,
#   col = 'khaki')

# Overlay GPS points 
# points(trainData$lon,
#        trainData$lat,
#        pch = 21,
#        bg = 'red',
#        cex = 1)

# Define background area by masking WORLDCLIM layers to just the KG zones with at 
# least 1 GPS record
# - First, we have to convert the KG zones containing GPS records back into an 'sf' object
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
kg_contain <- sf::st_as_sf(kg_contain)
kg_contain = sf::st_set_crs(kg_contain, crs_wgs84)

############################################################################
# Loop through all the predictor sets, and store background points for each
############################################################################

names(pred_choice)

for(p in 1:length(pred_choice) ){
  
  bg_area <- terra::mask(pred_choice[[p]], kg_contain)  
  
  #crs(pred_choice[[1]]) == crs(kg_contain)
  
  # Plot to check the mask worked
  # terra::plot(bg_area)
  
  # Sample random points from the background area defined by the KG zones occupied
  # - We use these background points as 'pseudo-absences' to test how well our climate
  #   model can distinguish between GPS points occupied by our focal species and these 
  #   'pseudo-absence' points 
  
  set.seed(2023)
  
  print(paste("Processing run", p, "of", length(pred_choice)))
  
  bg_points <- terra::spatSample(
    x = bg_area,        # Raster of background area to sample points from 
    size = 10000,        # How many background points do we want?
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
  
  bg_points = as.data.frame(bg_points) %>%
    # Keep only the lat/lon columns and key environmental variables
    dplyr::select(
      decimalLatitude = lat,
      decimalLongitude = lon,
      predictor_sets[[p]]
    ) %>%
    dplyr::mutate(species = "Diaphorina citri") %>%
    dplyr::select(
      species,
      everything()
    ) %>%
    # Drop rows with NA 
    tidyr::drop_na()
  
  # Reproject CRS 
  coordinates(bg_points) <-  ~ decimalLongitude + decimalLatitude
  crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
  slot(bg_points, "proj4string") <- crs_wgs84
  
  output_name = paste("background_points_", names(pred_choice[p]), sep = "") 
  assign(output_name, as.data.frame(bg_points) )
  
  # Check background points have been drawn from the correct geographic mask
  # terra::plot(kg_layer)
  # points(bg_points$lon,
  #        bg_points$lat,
  #        pch = 21,
  #        bg = 'red',
  #        cex = 1)
  
} # for

# outputs:

# entire range
background_points <- list(
  background_points_all_19_reduced_r2_VIF = background_points_all_19_reduced_r2_VIF
)

# invaded range
background_points_invaded_range <- list(
  background_points_all_19_reduced_r2_VIF = background_points_all_19_reduced_r2_VIF
)

# native range
background_points_native_range <- list(
  background_points_all_19_reduced_r2_VIF = background_points_all_19_reduced_r2_VIF
)

