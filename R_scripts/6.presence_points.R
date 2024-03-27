##################
# PRESENCE POINTS
##################

if (choice == "a") {
  pred_choice = predictor_spatrasters_entire
  message("Entire distribution range data set.")
} else if (choice == "n") {
  pred_choice = predictor_spatrasters_native_range
  message("Native range data set.")
} else if (choice == "i"){
    pred_choice = predictor_spatrasters_invaded
    message("Invaded range data set.")
  } else {
  # Optionally, handle a default case or provide an error message
  cat("Invalid training dataset specified.\n")
}

names(pred_choice)

for(p in 1:length(pred_choice) ){
  
  speciesEnv <- base::data.frame(
    raster::extract(pred_choice[[p]], cbind(training_dataset$lon, training_dataset$lat) )
  )
  
  speciesWd = cbind(training_dataset, speciesEnv)
  
  # Process data 
  speciesWd <- as.data.frame(speciesWd) %>%
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
  coordinates(speciesWd) <-  ~ decimalLongitude + decimalLatitude
  crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
  slot(speciesWd, "proj4string") <- crs_wgs84
  
  output_name = paste("presence_points_", names(pred_choice[p]), sep = "") 
  assign(output_name, as.data.frame(speciesWd))
  
}#for

# outputs:

presence_points <- list(
  #presence_points_all_19_all_preds = presence_points_all_19_all_preds,
  #presence_points_all_19_reduced_r2_preds = presence_points_all_19_reduced_r2_preds,
  #presence_points_all_19_reduced_r2_network_preds = presence_points_all_19_reduced_r2_network_preds
  #presence_points_all_19_reduced_VIF_preds = presence_points_all_19_reduced_VIF_preds
  presence_points_all_19_reduced_r2_VIF = presence_points_all_19_reduced_r2_VIF
)

presence_points$presence_points_all_19_reduced_r2_VIF
