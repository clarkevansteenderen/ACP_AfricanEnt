####################################################################################
# RUN THIS ONLY THE FIRST TIME TO CREATE ALL THE RELEVANT FOLDERS AND SUBFOLDERS
####################################################################################

if(!dir.exists("data")){
  dir.create("data")
  dir.create("data/gps")
}

if(!dir.exists("data/shapefiles")){
  dir.create("data/shapefiles")
  dir.create("data/shapefiles/koppen_geiger")
}

if(!dir.exists("data/environmental_layers")){
  dir.create("data/environmental_layers")
  dir.create("data/environmental_layers/2050")
  dir.create("data/environmental_layers/2070")
  dir.create("data/environmental_layers/current")
  dir.create("data/environmental_layers/2050/RCP4.5")
  dir.create("data/environmental_layers/2070/RCP4.5")
  dir.create("data/environmental_layers/2050/RCP8.5")
  dir.create("data/environmental_layers/2070/RCP8.5")
}