#######################################################
# DOWNLOAD CURRENT CLIMATE RASTER LAYERS
#######################################################

# Download the WORLDCLIM raster layers for current time period to your PC
# - This will download and store all 19 WORLDCLIM layers to a folder
#   of your choice (given using 'path = ...' below)
# - Raster layers are stored as 'SpatRaster' so they are compatible with the 
#   'terra' R package 

message("You only need to download climate data the first time you run this code.")
download_current_clim_data = readline("Download current climate data? y or n ")
download_current_clim_data = tolower(download_current_clim_data)
download_current_clim_data = substr(download_current_clim_data, 1,1)

if(download_current_clim_data == "y"){
  wc_current <- geodata::worldclim_global(
    var = "bio",
    res = 2.5,      # Minute degree resolution of raster layers
    path = here::here("./data/environmental_layers/current/"),
    version = "2.1"
  )
  message("Successfully downloaded")
} 

# Load the WORLDCLIM rasters layers we already have downloaded 
# - We don't need to run the download code above each new R session 
predictors <- terra::rast( list.files(
  here::here("./data/environmental_layers/current/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))  

#terra::plot(predictors)

#head(predictors)

# Plot each of the 19 WORLDCLIM layers to check they imported correctly 
# terra::plot(predictors)

# Set the CRS (coordinate reference system) projection for the current climate layers 
# - Use the correct wkt CRS format - no more PROJ4 strings! 
terra::crs(predictors) <- "epsg:4326"
# terra::crs(predictors, describe = T)

#######################################################
# DOWNLOAD SPECIES GPS DATA FROM GBIF 
#######################################################

# Below, we will download GPS data from GBIF for Diaphorina citri,
# - We can download records from GBIF, or import GPS records from a .csv 
#   file that we have stored on our PC somewhere 
# - Pick the option that works for you 

# Option #1: Download species occurrences (GPS) from GBIF
# set.seed(2012)
# sp_gps <- geodata::sp_occurrence(
#   genus = "Diaphorina",
#   species = "citri",
#   download = TRUE,
#   geo = TRUE,
#   removeZeros = TRUE,
#   nrecs = 2000    # Only download 2000 GPS - remove this for a proper analysis
# )
# head(sp_gps)

# Option #2: Alternatively, we could import a csv file containing GPS data 
sp_gps <- readr::read_csv("./data/gps/Diaphorina_citri.csv") %>%
  dplyr::select(
    species,
    lat = decimalLatitude,
    lon = decimalLongitude
  )

head(sp_gps)
nrow(sp_gps)
print("Successfully read in the GPS data file")

# Let's just keep the columns of data that we will use going forward 
sp_data <- sp_gps %>%
  dplyr::select(
    species,
    lon,
    lat
  )

#################################
# Remove duplicate GPS data 
#################################

sp_data <- sp_data %>%
  dplyr::distinct(lon, lat, .keep_all= TRUE)

# Convert one of our environmental predictors into a raster layer 
r <- raster::raster(predictors[[1]])

# Extract longitude and latitude into a dataframe
xy <- sp_data %>%
  dplyr::select(
    lon,
    lat
  ) %>%
  # Coerce into a data.frame object (can't be a tibble!!!)
  as.data.frame(.)
head(xy)

# Retain only 1 GPS record per grid cell
set.seed(2012)
sp_data_thin <- dismo::gridSample(
  xy = xy,     # Data.frame containing lon/lat columns only 
  r = r ,      # Environmental raster (must be a raster, not spatRast object)
  n = 1        # Number of records to keep per cell
)

nrow(sp_data)
nrow(sp_data_thin)



# split the data into training and testing sets

`%not_in%`<- Negate(`%in%`)
testData=dplyr::slice_sample(sp_data_thin, prop=.2)
trainData=dplyr::filter(sp_data_thin, lon %not_in% testData$lon & lat %not_in% testData$lat)

#################################
# Get world map 
#################################

world_map <- rnaturalearth::ne_countries(
  scale = "medium", 
  returnclass = "sf"
) 

# Plot GPS points on world map to check our locality data is correct 
global_distr = ggplot() +
  # Add raster layer of world map 
  geom_sf(data = world_map, alpha = 0.5) +
  # Add GPS points 
  geom_point(
    data = sp_data_thin, 
    size = 0.5,
    aes(
      x = lon, 
      y = lat,
      color = ifelse(lon > 60, "red", "black"),
      shape = ifelse(lon > 60, "triangle", "circle")
    )
  )  +
  scale_colour_manual(values = c("red", "black")) +
  # Set world map CRS 
  coord_sf(
    crs = 4326,
    expand = FALSE
  ) + 
  xlab("Longitude") + 
  ylab("Latitude")

global_distr

ggsave("global_distribution_map.png", global_distr, dpi = 450)
ggsave("global_distribution_map.svg", global_distr)

# Let's just keep the GPS records from the native range to build our climate model 
# i.e. everything right of 60 degrees E
# we'll later train the model on all the occurrence data, and compare them
species_native <- sp_data_thin %>%
  dplyr::filter(lon > 60)
nrow(species_native)

species_native_extent = sf::st_as_sf(species_native, coords = c("lon", "lat"), crs = 4326)
species_native_extent = sf::st_bbox(species_native_extent)

print(paste("There are", nrow(species_native), "GPS points in the native range.") )

predictors_native_range = raster::crop(predictors, species_native_extent)
plot(predictors_native_range[[1]])

# get just the invaded range (remove all native range GPS coordinates)
species_invaded = sp_data_thin %>%
  dplyr::filter(lon < 60)
nrow(species_invaded)

species_invaded_extent = sf::st_as_sf(species_invaded, coords = c("lon", "lat"), crs = 4326)
species_invaded_extent = sf::st_bbox(species_invaded_extent)

predictors_invaded_range = raster::crop(predictors, species_invaded_extent)
plot(predictors_invaded_range[[1]])

#################################
# How many points in the USA?
#################################

species_usa <- sp_data_thin %>%
  dplyr::filter(lon > -127, lon< -65, lat > -5, lat < 50)

print(paste("There are", nrow(species_usa), "GPS points in the USA.") )

#################################
# How many points in Brazil?
#################################

species_braz <- sp_data_thin %>%
  dplyr::filter(lon > -73.99, lon < -34.80, lat > -33.75, lat < 5.27)

print(paste("There are", nrow(species_braz), "GPS points in Brazil.") )

#################################
# How many points in Africa?
#################################

species_afr <- sp_data_thin %>%
  dplyr::filter(lon > -26, lon < 55, lat > -48, lat < 40)

print(paste("There are", nrow(species_afr), "GPS points in Africa.") )

#################################
# Replot the GPS points on world map to see that we kept the right GPS data 
#################################

ggplot() +
  # Add raster layer of world map 
  geom_sf(data = world_map) +
  # Add GPS points 
  geom_point(
    data = species_native, 
    col = "forestgreen",
    size = 0.8,
    aes(
      x = lon, 
      y = lat
    )
  )  +
  # Set world map CRS 
  coord_sf(
    crs = 4326,
    expand = FALSE
  )



#######################################################
# FUTURE CLIMATE DATA -> DOWNLOAD OR READ IN
#######################################################
crs_wgs84 <- sp::CRS(SRS_string = "EPSG:4326")

message("You only need to download future climate data the first time you run this code.")
download_future_clim_data = readline("Download future climate data (2050 and 2070)? y or n ")
download_future_clim_data = tolower(download_future_clim_data)
download_future_clim_data = substr(download_future_clim_data, 1,1)

if(download_future_clim_data == "y"){
  
  # DOWNLOAD FUTURE CLIMATE DATA FOR 2050 and 2070 -> both the RCP4.5 and RCP8.5 PROJECTIONS
  
  # RCP4.5 - 2050
  
  # Download the RCP4.5 layers for 2050 to PC
  Env2050_4.5 <- raster::getData(
    name = 'CMIP5',
    # Which raster layers should be downloaded? bio = all 19 WORLDCLIM layers
    var = 'bio',
    # Resolution of the raster layers (2.5 arc minutes)
    res = 2.5,
    # Which RCP model? (here we are downloading the ssp245 or rcp4.5 scenario)
    rcp = 45,
    # Which climate model do we want?
    model = 'MR',
    # Specify the year? (here we download the 2050 data)
    year = 50,
    # If the files are not locally available, it will download them for us
    # Set to download = TRUE, the first time you run the code to download the
    # files to your PC
    download = TRUE,
    # Set the file path to the folder where the raster layers should be
    # stored and/or loaded from if already downloaded
    path = here::here("data/environmental_layers/2050/RCP4.5/") # create these folders manually in Documents first
  )
  
  # don't run this if the rasters have already been downloaded
  # reorder the bio names, as they're ordered 1, 10, 11, 12, 13 instead of 1,2,3 etc.
  numerical_part = as.numeric(gsub('.*_', '', names(predictors)))
  predictor_names_ordered = names(predictors)[order(numerical_part)] ;predictor_names_ordered
  
  # BE SUPER CAREFUL ABOUT ASSIGNING THE NEW NAMES, AS THEY CAN BE IN THE WRONG ORDER!!
  # Make the names of the layers consistent with the current climate layers
  
  names(Env2050_4.5) <- predictor_names_ordered
  
  # note that the downloaded file names are "mr45bi501" to "mr45bi519". We want these to be the
  # same as the names "wc2.1_2.5m_bio_1". Here's a function to change the names of the files
  # in the folder
  
  folder <- "data/environmental_layers/2050/RCP4.5/cmip5/2_5m"
  files <- list.files(folder); files
  # start at position 2, because the first file is a zip folder
  for (i in 2:length(files)) {
    old_file <- file.path(folder, files[i])
    new_file <- file.path(folder, paste0(names(predictors)[i-1], ".tif"))
    file.rename(old_file, new_file)
  }
  
  # RCP4.5 - 2070
  
  # Download the RCP4.5 layers for 2070 to PC
  Env2070_4.5 <- raster::getData(
    name = 'CMIP5',
    # Which raster layers should be downloaded? bio = all 19 WORLDCLIM layers
    var = 'bio',
    # Resolution of the raster layers (2.5 arc minutes)
    res = 2.5,
    # Which RCP model? (here we are downloading the ssp245 or rcp4.5 scenario)
    rcp = 45,
    # Which climate model do we want?
    model = 'MR',
    # Specify the year? (here we download the 2070 data)
    year = 70,
    # If the files are not locally available, it will download them for us
    # Set to download = TRUE, the first time you run the code to download the
    # files to your PC
    download = TRUE,
    # Set the file path to the folder where the raster layers should be
    # stored and/or loaded from if already downloaded
    path = here::here("data/environmental_layers/2070/RCP4.5/")
  )
  
  # Make the names of the layers consistent with the current climate layers
  
  names(Env2070_4.5) <- predictor_names_ordered
  
  # note that the downloaded file names are "mr45bi501" to "mr45bi519". We want these to be the
  # same as the names "wc2.1_2.5m_bio_1". Here's a function to change the names of the files
  # in the folder
  
  folder <- "data/environmental_layers/2070/RCP4.5/cmip5/2_5m"
  files <- list.files(folder); files
  
  # start at position 2, because the first file is a zip folder
  for (i in 2:length(files)) {
    old_file <- file.path(folder, files[i])
    new_file <- file.path(folder, paste0(names(predictors)[i-1], ".tif"))
    file.rename(old_file, new_file)
  }
  
  # RCP8.5 - 2050
  
  # Download the RCP4.5 layers for 2050 to PC
  Env2050_8.5 <- raster::getData(
    name = 'CMIP5',
    # Which raster layers should be downloaded? bio = all 19 WORLDCLIM layers
    var = 'bio',
    # Resolution of the raster layers (2.5 arc minutes)
    res = 2.5,
    # Which RCP model? (here we are downloading the ssp245 or rcp4.5 scenario)
    rcp = 85,
    # Which climate model do we want?
    model = 'MR',
    # Specify the year? (here we download the 2050 data)
    year = 50,
    # If the files are not locally available, it will download them for us
    # Set to download = TRUE, the first time you run the code to download the
    # files to your PC
    download = TRUE,
    # Set the file path to the folder where the raster layers should be
    # stored and/or loaded from if already downloaded
    path = here::here("data/environmental_layers/2050/RCP8.5/")
  )
  
  # Make the names of the layers consistent with the current climate layers
  
  names(Env2050_8.5) <- predictor_names_ordered
  
  # note that the downloaded file names are "mr45bi501" to "mr45bi519". We want these to be the
  # same as the names "wc2.1_2.5m_bio_1". Here's a function to change the names of the files
  # in the folder
  
  folder <- "data/environmental_layers/2050/RCP8.5/cmip5/2_5m"
  files <- list.files(folder); files
  # start at position 2, because the first file is a zip folder
  for (i in 2:length(files)) {
    old_file <- file.path(folder, files[i])
    new_file <- file.path(folder, paste0(names(predictors)[i-1], ".tif"))
    file.rename(old_file, new_file)
  }
  
  # RCP8.5 - 2070
  
  # Download the RCP4.5 layers for 2050 to PC
  Env2070_8.5 <- raster::getData(
    name = 'CMIP5',
    # Which raster layers should be downloaded? bio = all 19 WORLDCLIM layers
    var = 'bio',
    # Resolution of the raster layers (2.5 arc minutes)
    res = 2.5,
    # Which RCP model? (here we are downloading the ssp245 or rcp4.5 scenario)
    rcp = 85,
    # Which climate model do we want?
    model = 'MR',
    # Specify the year? (here we download the 2050 data)
    year = 70,
    # If the files are not locally available, it will download them for us
    # Set to download = TRUE, the first time you run the code to download the
    # files to your PC
    download = TRUE,
    # Set the file path to the folder where the raster layers should be
    # stored and/or loaded from if already downloaded
    path = here::here("data/environmental_layers/2070/RCP8.5/")
  )
  
  # Make the names of the layers consistent with the current climate layers
  
  names(Env2070_8.5) <- predictor_names_ordered
  
  # note that the downloaded file names are "mr45bi501" to "mr45bi519". We want these to be the
  # same as the names "wc2.1_2.5m_bio_1". Here's a function to change the names of the files
  # in the folder
  
  folder <- "data/environmental_layers/2070/RCP8.5/cmip5/2_5m"
  files <- list.files(folder); files
  # start at position 2, because the first file is a zip folder
  for (i in 2:length(files)) {
    old_file <- file.path(folder, files[i])
    new_file <- file.path(folder, paste0(names(predictors)[i-1], ".tif"))
    file.rename(old_file, new_file)
  }
  
}#if


# if already downloaded, read in here:
Env2050_4.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2050/RCP4.5/cmip5/2_5m") ,
  full.names = TRUE,
  pattern = '.tif'
))
Env2050_4.5 = raster::brick(Env2050_4.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2050_4.5) <- crs_wgs84

# if already downloaded, read in here:
Env2070_4.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2070/RCP4.5/cmip5/2_5m") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2070_4.5 = raster::brick(Env2070_4.5)



# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2070_4.5) <- crs_wgs84

Env4.5 <- list(Env2050_4.5, Env2070_4.5); Env4.5

# if already downloaded, read in here:
Env2050_8.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2050/RCP8.5/cmip5/2_5m") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2050_8.5 = raster::brick(Env2050_8.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2050_8.5) <- crs_wgs84

# if already downloaded, read in here:
Env2070_8.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2070/RCP8.5/cmip5/2_5m") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2070_8.5 = raster::brick(Env2070_8.5)


# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2070_8.5) <- crs_wgs84

Env8.5 <- list(Env2050_8.5, Env2070_8.5) ;Env8.5


####################################
# DOWNLOAD USING GEODATA PACKAGE

Env2030_8.5 = geodata::cmip6_world(model = "AWI-CM-1-1-MR",
                     time = "2021-2040",
                     var = "bioc",
                     res = 2.5,
                     ssp = "585",
                     path = here::here("data/environmental_layers/2030_cmip6/RCP8.5/"))

Env2030_8.5 = raster::brick(Env2030_8.5)
crs(Env2030_8.5) <- crs_wgs84

Env2030_4.5 = geodata::cmip6_world(model = "AWI-CM-1-1-MR",
                                   time = "2021-2040",
                                   var = "bioc",
                                   res = 2.5,
                                   ssp = "245",
                                   path = here::here("data/environmental_layers/2030_cmip6/RCP4.5/"))

Env2030_4.5 = raster::brick(Env2030_4.5)
crs(Env2030_4.5) <- crs_wgs84

Env2050_8.5 = geodata::cmip6_world(model = "AWI-CM-1-1-MR",
                                   time = "2041-2060",
                                   var = "bioc",
                                   res = 2.5,
                                   ssp = "585",
                                   path = here::here("data/environmental_layers/2050_cmip6/RCP8.5/"))

Env2050_8.5 = raster::brick(Env2050_8.5)
crs(Env2050_8.5) <- crs_wgs84

Env2050_4.5 = geodata::cmip6_world(model = "AWI-CM-1-1-MR",
                                   time = "2041-2060",
                                   var = "bioc",
                                   res = 2.5,
                                   ssp = "245",
                                   path = here::here("data/environmental_layers/2050_cmip6/RCP4.5/"))

Env2050_4.5 = raster::brick(Env2050_4.5)
crs(Env2050_4.5) <- crs_wgs84

Env2070_8.5 = geodata::cmip6_world(model = "AWI-CM-1-1-MR",
                                   time = "2061-2080",
                                   var = "bioc",
                                   res = 2.5,
                                   ssp = "585",
                                   path = here::here("data/environmental_layers/2070_cmip6/RCP8.5/"))

Env2070_8.5 = raster::brick(Env2070_8.5)
crs(Env2070_8.5) <- crs_wgs84

Env2070_4.5 = geodata::cmip6_world(model = "AWI-CM-1-1-MR",
                                   time = "2061-2080",
                                   var = "bioc",
                                   res = 2.5,
                                   ssp = "245",
                                   path = here::here("data/environmental_layers/2070_cmip6/RCP4.5/"))

Env2070_4.5 = raster::brick(Env2070_4.5)
crs(Env2070_4.5) <- crs_wgs84

Env4.5 <- list(Env2030_4.5, Env2050_4.5, Env2070_4.5) ;Env4.5
Env8.5 <- list(Env2030_8.5, Env2050_8.5, Env2070_8.5) ;Env8.5


######################################
# if already downloaded, read in here:

##################
# 2030 RCP4.5
##################

Env2030_4.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2030_cmip6/RCP4.5/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2030_4.5 = raster::brick(Env2030_4.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2030_4.5) <- crs_wgs84

##################
# 2030 RCP8.5
##################
# if already downloaded, read in here:
Env2030_8.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2030_cmip6/RCP8.5/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2030_8.5 = raster::brick(Env2030_8.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2030_8.5) <- crs_wgs84

##################
# 2050 RCP4.5
##################
# if already downloaded, read in here:
Env2050_4.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2050_cmip6/RCP4.5/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2050_4.5 = raster::brick(Env2050_4.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2050_4.5) <- crs_wgs84

##################
# 2050 RCP8.5
##################
# if already downloaded, read in here:
Env2050_8.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2050_cmip6/RCP8.5/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2050_8.5 = raster::brick(Env2050_8.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2050_8.5) <- crs_wgs84

##################
# 2070 RCP4.5
##################
# if already downloaded, read in here:
Env2070_4.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2070_cmip6/RCP4.5/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2070_4.5 = raster::brick(Env2070_4.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2070_4.5) <- crs_wgs84

##################
# 2070 RCP8.5
##################
# if already downloaded, read in here:
Env2070_8.5 <- terra::rast( list.files(
  here::here("data/environmental_layers/2070_cmip6/RCP8.5/wc2.1_2.5m/") ,
  full.names = TRUE,
  pattern = '.tif'
))

Env2070_8.5 = raster::brick(Env2070_8.5)

# Set the CRS projection for the future climate layers 
# - Use the correct wkt CRS format - no more PROJ4
crs(Env2070_8.5) <- crs_wgs84


names(Env2030_4.5) = predictor_names_ordered
names(Env2030_8.5) = predictor_names_ordered

names(Env2050_4.5) = predictor_names_ordered
names(Env2050_8.5) = predictor_names_ordered

names(Env2070_4.5) = predictor_names_ordered
names(Env2070_8.5) = predictor_names_ordered

Env4.5 <- list(Env2030_4.5, Env2050_4.5, Env2070_4.5) ;Env4.5
Env8.5 <- list(Env2030_8.5, Env2050_8.5, Env2070_8.5) ;Env8.5

