#######################################################
# MULTICOLLINEARITY
#######################################################

# We start with all 19 predictors

# Extract climate values at focal taxon GPS points in the native range
# Focal taxon points are stored in `species_native`
head(species_native)

###############################################################
# Start with all 19 predictors
###############################################################

# Extract climate data at these points 
clim_sp <- terra::extract(
  x = predictors,          # SpatRast containing all 19 WORLDCLIM layers
  y = species_native               # SpatVect or data.frame containing GPS of study taxon (lon, lat)
)

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

cor_mat = cor(clim_sp_corr, method = "spearman")

# get rid of the correlations of 1 (comparing the same variable to itself, to remove diagonals)
# cor_mat[cor_mat == 1 ] <- 0
# 
# corrplot(cor_mat, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 0)

# Keep only correlations < 0.7
cor_mat[cor_mat < 0.7 ] <- 0

# Make an Igraph object from this matrix:
network <- graph_from_adjacency_matrix( cor_mat, weighted=T, mode="undirected", diag=F)

# Basic chart
plot(network,
     vertex.color = "yellow")

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
                                      subset = predictor_sets$all_19_reduced_r2 )

clim_sp_reduced <- terra::extract(
  x = reduced_preds_all19r2,         
  y = sp_data_thin              
)

clim_sp_reduced = dplyr::select(clim_sp_reduced, !ID)

#############
# Run VIF
#############

usdm::vifstep(clim_sp_reduced, th = 5)

########################################################################################
# All 19 reduced by PCMV, then R-squared:
########################################################################################
# Use Wang et al's predictors after reducing by PCMV (percentage contribution to model
# variance). These were: 4 5 6 10 11 14 15 16 18
# Now we use the correlation wheel method with R-squared at 0.7 to reduce these
########################################################################################

reduced_preds_all19pcmv = terra::subset(x = predictors, 
                                        subset = predictor_sets$wang )

clim_sp_reduced_wang <- terra::extract(
  x = reduced_preds_all19pcmv,         
  y = sp_gps              
)

clim_sp_reduced_wang = dplyr::select(clim_sp_reduced_wang, !ID)

###############################
# Run correlation wheel
###############################

corrWheel(clim_sp_reduced_wang, 0.7)

###################################################################3
# NOW CREATE THE SETS IN THE predictor_sets.R script