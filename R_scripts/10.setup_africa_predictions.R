#################################
# SET UP PREDICTIONS FOR AFRICA
#################################

##########################################################
# Get the user to choose the predictor set of interest
# for predicting. Writing a loop to go through all of these
# will take much too long
##########################################################

option_indices <- seq_along(predictor_spatrasters_entire)

# Display the options to the user
cat("Choose an option:\n")
for (i in option_indices) {
  cat(i, "-", names(predictor_spatrasters[i]), "\n")
}

# Prompt the user for the option number
user_choice <- readline("Enter the number of your choice: ")

# Retrieve the selected option as a spatraster
selected_predictors <- predictor_spatrasters_entire[[as.integer(user_choice)]]

# get the selected option as names of climate variables
climPred = predictor_sets[[as.integer(user_choice)]]

##########################################################

# Get map world map to project our model over
africa_ext <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(continent == "Africa")


# Mask rasters
africa_env_layers <- raster::crop(
  selected_predictors, 
  raster::extent(africa_ext)
)

# change this to RasterBrick
africa_env_layers = raster::brick(africa_env_layers)

crs_wgs84 <- sp::CRS(SRS_string = "EPSG:4326")
crs(africa_env_layers) <- crs_wgs84

#plot(africa_env_layers)

#user_dir = tcltk::tk_choose.dir(caption = "SELECT a directory to write/fetch current Africa climate data (e.g. LITERATURE/wang_set/africa_env_layers): ")
user_dir = readline("Directory to write current Africa climate data (e.g. LITERATURE/wang_set/africa_env_layers): ")
user_dir = user_dir
user_dir_2010 = paste(user_dir, "/2010/", sep = "")

if(!dir.exists(user_dir_2010)){
  
  dir.create(user_dir_2010)
  message(paste("Writing 2010 climate data to ", user_dir_2010))
  
  #Loop over each climate variable and save it with the correct name
  for (i in climPred){
    raster::writeRaster(
      africa_env_layers[[which(names(africa_env_layers) %in% i)]],
      filename = paste0(user_dir_2010, i, ".grd"),
      bylayer = TRUE,
      overwrite = TRUE,
      format = "raster",
      bandorder = 'BIL'
    )
  }
  
  message("Done :)")
}# if

##########################################################
# FUTURE CLIMATE DATA
##########################################################

# *********************************************
# Environmental rasters - RCP4.5 - 2050
# *********************************************

# Subset to only keep reduced predictor set
africa_env_layers <- raster::subset(Env2050_4.5, climPred) 

# Mask rasters to keep only the prediction region
africa_env_layers <- raster::crop( africa_env_layers, 
                                   raster::extent(africa_ext) )

# Set the CRS
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
crs(africa_env_layers) <- crs_wgs84

# this was only useful when future clim data was downloaded the old way
# not necessary if geodata cmip6 was used -> more updated method
already_downloaded_future_clim = readline("Have you already downloaded future climate data (2050 and 2070)? y or n ")
already_downloaded_future_clim = tolower(already_downloaded_future_clim)
already_downloaded_future_clim = substr(already_downloaded_future_clim, 1,1)


if(!dir.exists(paste(user_dir, "/2050/RCP4.5", sep = ""))){
  
  dir.create(paste(user_dir, "/2050", sep = ""))
  dir.create(paste(user_dir, "/2050/RCP4.5", sep = ""))
  
  user_dir_2050_RCP4.5 = paste(user_dir, "/2050/RCP4.5/", sep = "")
  
  message(paste("Writing 2050 RCP4.5 climate data to ", user_dir_2050_RCP4.5))
  
  # Loop over each climate variable and save it with the correct name, hopefully
  
  for (i in climPred){
    raster::writeRaster(
      africa_env_layers[[which(names(africa_env_layers) %in% i)]], 
      filename = paste0(
        user_dir_2050_RCP4.5, i, ".grd"
      ), 
      bylayer = TRUE,
      overwrite = TRUE, 
      format = "raster",
      bandorder = 'BIL'
    )
  }
  
  message("Done :)")
  
} else{
  user_dir_2050_RCP4.5 = paste(user_dir, "/2050/RCP4.5/", sep = "")
}


# *********************************************
# Environmental rasters - RCP8.5 - 2050
# *********************************************

# Subset to only keep reduced predictor set
africa_env_layers <- raster::subset(Env2050_8.5, climPred) 

# Mask rasters to keep only the prediction region
africa_env_layers <- raster::crop( africa_env_layers, 
                                  raster::extent(africa_ext) )

# Set the CRS
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
crs(africa_env_layers) <- crs_wgs84



if(!dir.exists(paste(user_dir, "/2050/RCP8.5", sep = ""))){
  
  dir.create(paste(user_dir, "/2050/RCP8.5", sep = ""))
  
  user_dir_2050_RCP8.5 = paste(user_dir, "/2050/RCP8.5/", sep = "")
  
  message(paste("Writing 2050 RCP8.5 climate data to ", user_dir_2050_RCP8.5))
  
  # Loop over each climate variable and save it with the correct name, hopefully
  
  for (i in climPred){
    raster::writeRaster(
      africa_env_layers[[which(names(africa_env_layers) %in% i)]], 
      filename = paste0(
        user_dir_2050_RCP8.5, i, ".grd"
      ), 
      bylayer = TRUE,
      overwrite = TRUE, 
      format = "raster",
      bandorder = 'BIL'
    )
  }
  
  message("Done :)")
  
} else{
  user_dir_2050_RCP8.5 = paste(user_dir, "/2050/RCP8.5/", sep = "")
}


# *********************************************
# Environmental rasters - RCP4.5 - 2070
# *********************************************

# Subset to only keep reduced predictor set
africa_env_layers <- raster::subset(Env2070_4.5, climPred) 

# Mask rasters to keep only the prediction region
africa_env_layers <- raster::crop( africa_env_layers, 
                                  raster::extent(africa_ext) )

# Set the CRS
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
crs(africa_env_layers) <- crs_wgs84


if(!dir.exists(paste(user_dir, "/2070/RCP4.5", sep = ""))){
  
  dir.create(paste(user_dir, "/2070", sep = ""))
  dir.create(paste(user_dir, "/2070/RCP4.5", sep = ""))
  
  user_dir_2070_RCP4.5 = paste(user_dir, "/2070/RCP4.5/", sep = "")
  
  message(paste("Writing 2070 RCP4.5 climate data to ", user_dir_2070_RCP4.5))
  
  # Loop over each climate variable and save it with the correct name, hopefully
  
  for (i in climPred){
    raster::writeRaster(
      africa_env_layers[[which(names(africa_env_layers) %in% i)]], 
      filename = paste0(
        user_dir_2070_RCP4.5, i, ".grd"
      ), 
      bylayer = TRUE,
      overwrite = TRUE, 
      format = "raster",
      bandorder = 'BIL'
    )
  }
  
  message("Done :)")
  
} else{
  user_dir_2070_RCP4.5 = paste(user_dir, "/2070/RCP4.5/", sep = "")
}


# *********************************************
# Environmental rasters - RCP8.5 - 2070
# *********************************************

# Subset to only keep reduced predictor set
africa_env_layers <- raster::subset(Env2070_8.5, climPred) 

# Mask rasters to keep only the prediction region
africa_env_layers <- raster::crop( africa_env_layers, 
                                  raster::extent(africa_ext) )

# Set the CRS
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
crs(africa_env_layers) <- crs_wgs84

if(!dir.exists(paste(user_dir, "/2070/RCP8.5", sep = ""))){
  
  dir.create(paste(user_dir, "/2070/RCP8.5", sep = ""))
  
  user_dir_2070_RCP8.5 = paste(user_dir, "/2070/RCP8.5/", sep = "")
  
  message(paste("Writing 2070 RCP8.5 climate data to ", user_dir_2070_RCP8.5))
  
  # Loop over each climate variable and save it with the correct name, hopefully
  
  for (i in climPred){
    raster::writeRaster(
      africa_env_layers[[which(names(africa_env_layers) %in% i)]], 
      filename = paste0(
        user_dir_2070_RCP8.5, i, ".grd"
      ), 
      bylayer = TRUE,
      overwrite = TRUE, 
      format = "raster",
      bandorder = 'BIL'
    )
  }
  
  message("Done :)")
  
} else{
  user_dir_2070_RCP8.5 = paste(user_dir, "/2070/RCP8.5/", sep = "")
}


# *********************************************
# Environmental rasters - RCP4.5 - 2030
# *********************************************

# Subset to only keep reduced predictor set
africa_env_layers <- raster::subset(Env2030_4.5, climPred) 

# Mask rasters to keep only the prediction region
africa_env_layers <- raster::crop( africa_env_layers, 
                                   raster::extent(africa_ext) )

# Set the CRS
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
crs(africa_env_layers) <- crs_wgs84


if(!dir.exists(paste(user_dir, "/2030/RCP4.5", sep = ""))){
  
  dir.create(paste(user_dir, "/2030/RCP4.5", sep = ""))
  
  user_dir_2030_RCP4.5 = paste(user_dir, "/2030/RCP4.5/", sep = "")
  
  message(paste("Writing 2030 RCP4.5 climate data to ", user_dir_2030_RCP4.5))
  
  # Loop over each climate variable and save it with the correct name, hopefully
  
  for (i in climPred){
    raster::writeRaster(
      africa_env_layers[[which(names(africa_env_layers) %in% i)]], 
      filename = paste0(
        user_dir_2030_RCP4.5, i, ".grd"
      ), 
      bylayer = TRUE,
      overwrite = TRUE, 
      format = "raster",
      bandorder = 'BIL'
    )
  }
  
  message("Done :)")
  
} else{
  user_dir_2030_RCP4.5 = paste(user_dir, "/2030/RCP4.5/", sep = "")
}


# *********************************************
# Environmental rasters - RCP8.5 - 2030
# *********************************************

# Subset to only keep reduced predictor set
africa_env_layers <- raster::subset(Env2030_8.5, climPred) 

# Mask rasters to keep only the prediction region
africa_env_layers <- raster::crop( africa_env_layers, 
                                   raster::extent(africa_ext) )

# Set the CRS
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
crs(africa_env_layers) <- crs_wgs84


if(!dir.exists(paste(user_dir, "/2030/RCP8.5", sep = ""))){
  
  dir.create(paste(user_dir, "/2030/RCP8.5", sep = ""))
  
  user_dir_2030_RCP8.5 = paste(user_dir, "/2030/RCP8.5/", sep = "")
  
  message(paste("Writing 2030 RCP8.5 climate data to ", user_dir_2030_RCP8.5))
  
  # Loop over each climate variable and save it with the correct name, hopefully
  
  for (i in climPred){
    raster::writeRaster(
      africa_env_layers[[which(names(africa_env_layers) %in% i)]], 
      filename = paste0(
        user_dir_2030_RCP8.5, i, ".grd"
      ), 
      bylayer = TRUE,
      overwrite = TRUE, 
      format = "raster",
      bandorder = 'BIL'
    )
  }
  
  message("Done :)")
  
} else{
  user_dir_2030_RCP8.5 = paste(user_dir, "/2030/RCP8.5/", sep = "")
}
