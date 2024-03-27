#######################################################
# MULTICOLLINEARITY
#######################################################

training_dataset = readline("Use all data (A), invaded range (I), or native range only (N)? ")
choice = tolower(training_dataset)

if (choice == "a") {
  training_dataset = sp_data_thin
  message("Entire distribution range data set.")
} else if (choice == "n") {
  training_dataset = species_native
  message("Native range data set.")
} else if (choice == "i") {
  training_dataset = species_invaded
  message("Invaded range data set.") 
} else {
  # Optionally, handle a default case or provide an error message
  cat("Invalid training dataset specified.\n")
}

# We start with all 19 predictors

# Extract climate values at focal taxon GPS points 

head(training_dataset)

###############################################################
# Start with all 19 predictors
###############################################################

# Extract climate data at these points 
clim_sp <- terra::extract(
  x = predictors,          # SpatRast containing all 19 WORLDCLIM layers
  y = training_dataset               # SpatVect or data.frame containing GPS of study taxon (lon, lat)
)

head(clim_sp)
clim_sp = na.omit(clim_sp)

clim_sp = dplyr::select(clim_sp, !ID)
head(clim_sp)

######################################################
# All 19 reduced by R-squared
# Using the correlation wheel approach
######################################################
options(encoding = "latin1")
source("correlation_wheel.R")

corwh = corrWheel(Data = clim_sp, Threshold = 0.7)# Specify the R2 value we want as our lowest limit here

###########################
# make a correlation matrix
###########################

clim_sp_corr = clim_sp
new_colnames <- gsub(".*_(\\d+)$", "\\1", names(clim_sp_corr))
names(clim_sp_corr) <- new_colnames

cor_mat = cor(clim_sp_corr, method = "pearson")

# get rid of the correlations of 1 (comparing the same variable to itself, to remove diagonals)
# cor_mat[cor_mat == 1 ] <- 0
# 
# corrplot(cor_mat, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 0)

# Keep only correlations < 0.7
cor_mat[cor_mat < 0.7 ] <- 0

# Make an Igraph object from this matrix:
network <- igraph::graph_from_adjacency_matrix( cor_mat, weighted=T, mode="undirected", diag=F)

# Basic chart
par(bg = "white")
plot(network,
     vertex.color = "yellow")

# Let's choose these predictors for the native range dataset:
# bio1, bio5, bio7, bio12, bio14

# Let's choose these predictors for the entire distribution range dataset:
# bio1, bio5, bio7, bio12, bio19

######################################################
# All 19 reduced by VIF
######################################################

# Identify collinear variables that should be excluded (VIF > 5)
# VIF of 5 might be best
usdm::vifstep(clim_sp, th = 5)

######################################################
# All 19 reduced by R-squared, then VIF
######################################################

reduced_preds_all19r2 = terra::subset(x = predictors, 
                                      subset = predictor_sets$all_19_reduced_r2_VIF )

clim_sp_reduced <- terra::extract(
  x = reduced_preds_all19r2,         
  y = training_dataset              
)

clim_sp_reduced = dplyr::select(clim_sp_reduced, !ID)

#############
# Run VIF
#############

usdm::vifstep(clim_sp_reduced, th = 5)

##################################################################
# NOW CREATE THE SETS IN THE predictor_sets.R script

