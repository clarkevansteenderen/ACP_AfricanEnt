#######################################################
# MODEL TUNING
#######################################################

# We need a data.frame of the lon and lat (in that order) for our background points 
# you can pick any background_points object, because they all have the exact same set of GPS coordinates,
# the only difference is the differing sets of column predictors

##########################################################
# Get the user to choose the predictor set of interest
# for model tuning. Writing a loop to go through all of these
# will take much too long
##########################################################

option_indices <- seq_along(pred_choice)

# Display the options to the user
cat("Choose an option:\n")
for (i in option_indices) {
  cat(i, "-", names(pred_choice[i]), "\n")
}

# Prompt the user for the option number
user_choice <- readline("Enter the number of your choice: ")

# Retrieve the selected option
selected_option <- pred_choice[[as.integer(user_choice)]]

##########################################################


bg_pts <- background_points[[as.numeric(user_choice)]] %>%
  dplyr::select(
    lon = decimalLongitude , 
    lat = decimalLatitude
  )

nrow(bg_pts)
nrow(training_dataset)

head(bg_pts)
head(training_dataset)

# We need a list of the feature class (fc) and regularisation multipliers (rm) to test
list_settings <- list(
  fc = c("L","Q","H","LQH"), 
  rm = c(1,2,4,6,8)
)

# Run model tuning experiments 

# Set reproducible seed
set.seed(2023)

dismo::maxent()

# Run model tuning 
# this can take a long time to run!
# with 16GB RAM, all 19 predictors now take about 191 mins

print("RUNNING MODEL TUNING NOW")

tuning_results <- 
  ENMeval::ENMevaluate(
    occs = training_dataset,
    envs = selected_option, # this is what you change in each run, needs to be a spatraster
    bg = bg_pts,
    tune.args = list_settings, 
    partitions = "block",
    algorithm = "maxent.jar",
    doClamp = FALSE
  )

tuning_results
str(tuning_results, max.level=2)

# have a look at all the output for each combination
tuning_results@results
tuning_results@results.partitions

# Visualise results 

# Plot the model tuning results

ENMeval::evalplot.stats(
  e = tuning_results,              # Variable containing ENMevaluate results 
  stats = c(                       # Which metrics to plot?
    "auc.val",                     # - Make a plot for AUC
    "or.mtp",                      # - Make a plot for omission rate (minimum training presence)
    "or.10p",                       # - Make a plot for omission rate (10th percentile)
    "cbi.val"
    ),   
  color = "fc",                    # Colours lines/bars by feature class (fc)
  x.var = "rm",                    # Variable to plot on x-axis
  error.bars = FALSE               # Don't show error bars 
)
?ENMeval::evalplot.stats

# Select the optimal model settings 

# Extract the model tuning results to a data.frame 

res <- ENMeval::eval.results(tuning_results)
head(res)
res = na.omit(res)

subset_res <- res %>%
  select(rm, fc, auc.val.avg, auc.diff.avg, cbi.val.avg, or.10p.avg, AICc)
head(subset_res)

df_long <- subset_res %>%
  gather(key = "variable", value = "value", -rm, -fc)
df_long$rm = as.factor(df_long$rm)
head(df_long)

write.csv(df_long, file = "model_tuning_enmeval.csv", quote = FALSE, row.names = FALSE)
?write.csv

df_long = read.csv("models_full_range_trained/all19_reduced_r2_VIF/model_tuning_enmeval.csv")

tuning_results_ggplot = ggplot(df_long, aes(x = rm, y = value, color = fc, group = fc)) +
  geom_line() +
  geom_point(aes(shape = fc)) +
  scale_shape_manual(values = c(15, 16, 17, 5)) +
  facet_wrap(~variable, scales = "free_y", nrow = 4) +
  scale_colour_manual(values = c("black", "red", "royalblue", "forestgreen")) +
  labs(x = "Regularisation multiplier", y = "Value") +
  theme_classic() ;tuning_results_ggplot

ggsave("tuning_results_gg.svg", tuning_results_ggplot, width = 8, height = 7)

# plot the measures individually

# AUC test
ggplot(subset_res, aes(x = rm, y = auc.val.avg, color = fc, group = fc)) +
  geom_line() +
  geom_point() +
  ylim(0.5,1) +
  geom_hline(yintercept = 0.75, col = "grey", lty = 2) +
  labs(x = "Regularisation multiplier", y = "AUCtest") +
  theme_classic()

# AUC diff
ggplot(subset_res, aes(x = rm, y = auc.diff.avg, color = fc, group = fc)) +
  geom_line() +
  geom_point() +
  ylim(0,0.3) +
  labs(x = "Regularisation multiplier", y = "AUCdiff") +
  theme_classic()

# cbi.val.avg
ggplot(subset_res, aes(x = rm, y = cbi.val.avg, color = fc, group = fc)) +
  geom_line() +
  geom_point() +
  ylim(0,0.6) +
  labs(x = "Regularisation multiplier", y = "CBI") +
  theme_classic()

# or.10p.avg
ggplot(subset_res, aes(x = rm, y = or.10p.avg, color = fc, group = fc)) +
  geom_line() +
  geom_point() +
  ylim(0,0.3) +
  geom_hline(yintercept = 0.1, col = "grey", lty = 2) +
  labs(x = "Regularisation multiplier", y = "OR10") +
  theme_classic()

# AICc
ggplot(subset_res, aes(x = rm, y = AICc, color = fc, group = fc)) +
  geom_line() +
  geom_point() +
  ylim(6000, 7500) +
  #geom_hline(yintercept = 0.1, col = "grey", lty = 2) +
  labs(x = "Regularisation multiplier", y = "AICc") +
  theme_classic()

###############################
# FIND THE BEST MODELS
###############################

# Select the model settings (RM and FC) that optimised AICc (delta AICc == 0)
# LQH2 native range and entire distrib
best_delta_aicc <- res %>% 
  dplyr::filter(delta.AICc == 0)

svg(paste(getwd(), "/LQH2_dismo_plots.svg", sep = ""))
dismo::response(ENMeval::eval.models(tuning_results)[[best_delta_aicc$tune.args]])
dev.off()

best_delta_aicc_output = best_delta_aicc %>%
  t(.)

write.table(best_delta_aicc_output,
            "best_model_aicc.txt",
            #best_model_path,
            quote = FALSE)

# select the model that optimised AUC (highest AUC value)
  # H 4 native range
# H1 entire distrib
best_auc_test <- res %>% 
  dplyr::filter(auc.val.avg == max(auc.val.avg))

svg(paste(getwd(), "/H1_dismo_plots.svg", sep = ""))
dismo::response(ENMeval::eval.models(tuning_results)[[best_auc_test$tune.args]])
dev.off()

best_auc_test_output = best_auc_test %>%
  t(.)

write.table(best_auc_test_output,
            "best_model_auctest.txt",
            #best_model_path,
            quote = FALSE)

# select the model that optimised CBI (highest CBI value)
# LQH 1 native range
# LQH 6 entire distrib
best_cbi.val.avg <- res %>% 
  dplyr::filter(cbi.val.avg == max(cbi.val.avg)) 

svg(paste(getwd(), "/LQH6_dismo_plots.svg", sep = ""))
dismo::response(ENMeval::eval.models(tuning_results)[[best_cbi.val.avg$tune.args]])
dev.off()


best_cbi.val.avg_output = best_cbi.val.avg %>%
  t(.)

write.table(best_cbi.val.avg_output,
            "best_model_cbi.txt",
            #best_model_path,
            quote = FALSE)

# select the model that optimised the 10% omission rate (lowest or.10p value)
# L 8 native range
# LQH 8 entire distrib
best_or.10p.avg <- res %>% 
  dplyr::filter(or.10p.avg == min(or.10p.avg))

svg(paste(getwd(), "/LQH8_dismo_plots.svg", sep = ""))
dismo::response(ENMeval::eval.models(tuning_results)[[best_or.10p.avg$tune.args]])
dev.off()


best_or.10p.avg_output = best_or.10p.avg %>%
  t(.)

write.table(best_or.10p.avg_output,
            "best_model_or.10p.txt",
            #best_model_path,
            quote = FALSE)

# default output
default_mod_results <- res %>% 
  dplyr::filter(tune.args == "fc.LQH_rm.1") 

default_mod_results = default_mod_results %>%
  t(.)

write.table(default_mod_results,
            "best_model_default.txt",
            #best_model_path,
            quote = FALSE)


m1.mxjar <- eval.models(tuning_results)[["fc.LQH_rm.2"]]
m1.mxjar@lambdas

# devtools::install_github("johnbaums/rmaxent")
rmaxent::parse_lambdas(m1.mxjar)

# write the results to a text file

#best_model_directory = choose.dir(caption = "Select the folder to which to save the best model as a text file")
#best_model_path = paste(best_model_directory, "/best_model.txt", sep = "")

# Evaluate the best model  

# Let's evaluate the best model 
mod_best <- ENMeval::eval.models(tuning_results)[[best_delta_aicc$tune.args]]
mod_best
# Plot the marginal response curves for the predictor variables wit non-zero 
# coefficients in our model. We define the y-axis to be the cloglog transformation, which
# is an approximation of occurrence probability (with assumptions) bounded by 0 and 1
# (Phillips et al. 2017).

# par(bg = "#FFFFFF")
# 
# png(paste(best_model_directory, "/dismo_plots.png", sep = ""))
# svg(paste(best_model_directory, "/dismo_plots.svg", sep = ""))
# dismo_plots = dismo::response(ENMeval::eval.models(tuning_results)[[best_delta_aicc$tune.args]])
# dev.off()

# bio4 and bio6 appear to be the most informative, exclude the others

# You interpret these graphs to see if the relationship between your study species being 
# present at a site correlates with the environmental variables, and whether the shape of 
# the relationship makes sense for the species

# For example, if we consider bio12 (mean annual precipitation),
# - We can see that there is a relatively weak effect on our study species, with the species 
#   less likely to be recorded with increasing mean annual precipitation

# Another example, if we consider bio15 (coefficient of variation in seasonality precipitation),
# or how variable precipitation is between seasons,
# - We can see that the suitability for the species is very high when rainfall is not very variable 
#   between seasons (x-axis between 0 and 40), and as the variation in rainfall between seasons
#   increases (larger x-axis values), the suitability for our study species decreases.
#   - This would imply that our study species likes consistent rainfall (or a lack of rainfall) 
#     throughout the year

# Use kuenm package to calculate pROC curve -> assess model predictive accuracy

##############################################################################
# use the blockCV R package to partition data in testing and training sets
##############################################################################

predictor_rasterstack = raster::stack(selected_option)

# combine presences and absences

ABS_pts = background_points[[1]] %>% 
  dplyr::select(decimalLatitude, decimalLongitude)
ABS_pts$occ = 0

PRES_pts = presence_points[[1]] %>%
  dplyr::select(decimalLatitude, decimalLongitude)
PRES_pts$occ = 1

?ENMeval::get.block
?ENMeval::get.randomkfold

block_cross_validation = ENMeval::get.block(occs = PRES_pts, bg = ABS_pts)
# alternative method, where you specify the number of folds
block_cross_validation = ENMeval::get.randomkfold(occs = PRES_pts, bg = ABS_pts, kfolds = 5)
table(block_cross_validation$occs.grp)

PA_pts = rbind(ABS_pts, PRES_pts)

PA_pts_sf = sf::st_as_sf(PA_pts, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Convert one of our environmental predictors into a raster layer -> bio1 
r <- raster::raster(predictors[[1]])

occs.z = presence_points$presence_points_all_19_reduced_r2_VIF[-1]
colnames(occs.z)[1] = "latitude"
colnames(occs.z)[2] = "longitude"
occs.z = occs.z %>% dplyr::relocate(longitude, .before = latitude)
head(occs.z)

bg.z = background_points$background_points_all_19_reduced_r2_VIF[-1]
colnames(bg.z)[1] = "latitude"
colnames(bg.z)[2] = "longitude"
bg.z = bg.z %>% dplyr::relocate(longitude, .before = latitude)
head(bg.z)

#  "MESS" stands for Multivariate Environmental Similarity Surfaces
#Ideally, you want MESS values close to 0, indicating that the testing data's 
# environmental conditions are similar to those in the training data, 
# as this is more likely to result in reliable model evaluation

MESS_plot = ENMeval::evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", 
                              occs.z = occs.z, 
                              bg.z = bg.z, 
                              occs.grp = block_cross_validation$occs.grp, 
                              bg.grp = block_cross_validation$bg.grp) ;MESS_plot

ggplot2::ggsave("mess_plot.svg", MESS_plot)

# get actual mess values for each partition
MESS_plot$data

ggplot2::ggplot(data = MESS_plot$data, aes(x = partition, y = mess, fill = partition)) +
  geom_boxplot() +
  scale_fill_manual(values = c("firebrick2", "dodgerblue2", "green3", "darkorchid", "orange")) +
  geom_hline(yintercept = 1, col = "grey", lty = 2) +
  ylab("MESS value") +
  xlab("Partition block")

ENMeval::evalplot.envSim.hist(sim.type = "most_diff", ref.data = "occs", 
                              occs.z = occs.z, 
                              bg.z = bg.z,
                              occs.grp = block_cross_validation$occs.grp, 
                              bg.grp = block_cross_validation$bg.grp)                                          


block_partition = ENMeval::evalplot.grps(pts=occs.z, 
                       pts.grp = block_cross_validation$occs.grp, 
                       envs = raster::stack( predictors) )


ggplot2::ggsave("block_partition.svg", block_partition)


# evalplot.envSim.map(sim.type = "mess", ref.data = "occs", 
#                     envs = raster::stack( predictors_native_range), occs.z = occs.z, 
#                     bg.z = bg.z, 
#                     occs.grp = block_cross_validation$occs.grp, 
#                     bg.grp = block_cross_validation$bg.grp, 
#                     bb.buf = 7)


dismo_mess = dismo::mess(x = raster::stack(pred_choice$all_19_reduced_r2_VIF),
            v = occs.z[,-c(1:2)])

# Get map of South Africa to project our model over
africa_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(continent == "Africa") %>%
  st_as_sf() %>%
  st_transform(., 4326)


r.mess.mask <- dismo_mess>0
#plot(r.mess.mask)

test <- r.mess.mask
test <- raster::crop(test, africa_map)
plot(test)
test <- raster::mask(test, africa_map)
plot(test)

# Convert from raster to data frame
rcp <- rasterToPoints(test)
rcpdf <- data.frame(rcp)
head(rcpdf)
colnames(rcpdf) <- c("Longitude", "Latitude", "Cat")
rcpdf$Cat <- as.factor(rcpdf$Cat)


# Plot binary map
mess_extrap_plot <- ggplot(data=rcpdf) +
  geom_tile(data = rcpdf, aes(x = Longitude,
                              y = Latitude, 
                              fill=Cat)) +
  scale_fill_manual(values=c("gray90", "gray20"),
                    labels = c("Extrapolate", "Interpolate")) +
  geom_sf(data = africa_map, fill = NA) +
  labs(fill = "MESS") + 
  annotation_scale(location = "bl", # 'br' = bottom right
                   style = "ticks", 
                   width_hint = 0.2) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0.1, "in"), 
                         pad_y = unit(0.2, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(ylim = c(-35, 38), xlim = c(-25, 55)) +
  theme_opts +
  theme(legend.text = element_text(size = 10),
        legend.position = c(0.2, 0.3))

mess_extrap_plot

ggsave("mess_extrap_interpol_africa.png", mess_extrap_plot, width = 5, height = 6)

################################
# blockCV functions -> optional
################################

######################################
# SPATIAL BLOCKS METHOD
######################################

sb1 = blockCV::cv_spatial(x = PA_pts_sf,
                    column = "occ",
                    k = 5,
                    size = 350000,
                    selection = "random",
                    iteration = 50,
                    biomod2 = TRUE)

sb2 = blockCV::cv_spatial(x = PA_pts_sf,
                          column = "occ",
                          r = predictors_native_range,
                          hexagon = FALSE,
                          k = 5,
                          size = 350000,
                          selection = "random",
                          iteration = 50,
                          biomod2 = TRUE)

?blockCV::cv_spatial

tmap::tm_shape(sb2$blocks) +
  tmap::tm_fill(col = "folds", style = "cat")

# plot on the map
blockCV::cv_plot(cv = sb1, r = predictors_native_range,
                 raster_colors = terrain.colors(10, alpha = 0.5),
                 label_size = 4)

######################################
# SPATIAL AND ENVIRONMENTAL CLUSTERING
######################################
set.seed(6)
#spatial clustering
scv <- blockCV::cv_cluster(x = PA_pts_sf,
                           column = "occ", # optional: counting number of train/test records
                           k = 5)


# plot maps
blockCV::cv_plot(cv = scv, 
                 x = PA_pts_sf)

# environmental clustering
ecv <- blockCV::cv_cluster(x = PA_pts_sf,
                           column = "occ",
                           r = pred_choice$all_19_reduced_r2_VIF_full_range_preds,
                           k = 5, 
                           scale = TRUE)

# plot maps
blockCV::cv_plot(cv = ecv, 
                 x = PA_pts_sf) +
  scale_color_manual(values=c("forestgreen", "grey"))


environ_clustering_plot = blockCV::cv_similarity(cv = ecv, # the environmental clustering
                       x = PA_pts_sf, 
                       r = pred_choice$all_19_reduced_r2_VIF_full_range_preds, 
                       progress = TRUE) +
  ggtitle("(a) Environmental clustering")


#ggsave("environmental_clustering_mess.svg", environ_clustering_plot)

spatial_clustering_plot = blockCV::cv_similarity(cv = scv, # the spatial clustering
                       x = PA_pts_sf, 
                       r = pred_choice$all_19_reduced_r2_VIF_preds, 
                       progress = FALSE) +
  ggtitle("(b) Spatial clustering")


#ggsave("spatial_clustering_mess.svg", spatial_clustering_plot)

mess_clustering = gridExtra::grid.arrange(environ_clustering_plot, spatial_clustering_plot)
ggsave("mess_clustering.svg", mess_clustering, width = 5, height = 6)

#MESS represents how similar a point in a testing fold is to a training fold 
#(as a reference set of points), with respect to a set of predictor variables in r. 
#The negative values are the sites where at least one variable has a value that is outside 
#the range of environments over the reference set, so these are novel environments.

spatial_corr = blockCV::cv_spatial_autocor(x = PA_pts_sf, column = "occ")
spatial_corr$range/1000 # spatial autocorrelation range is about 272 km (native range) and 770 km (entire distribution)

# gamma_vals = spatial_corr$variograms[[1]]$exp_var$gamma
# dist_vals = spatial_corr$variograms[[1]]$exp_var$dist
# spatial_corr_plot_vals = as.data.frame( cbind(gamma_vals, dist_vals) )
# 
# ggplot2::ggplot(data = spatial_corr_plot_vals,
#                 aes(x = dist_vals, y = gamma_vals)) +
#   geom_smooth()

autocor_variogram = plot(spatial_corr$variograms[[1]])
# the sill and range is where the peak levels out -> i.e. where spatial autocorrelation is no longer a problem

svg(paste(getwd(), "/autocorrelation.svg", sep = ""))
autocor_variogram
dev.off()

# using ecospat

ecospat::ecospat.mantel.correlogram(dfvar = presence_points$presence_points_all_19_reduced_r2_VIF[2:7],
                                    colxy = 1:2,
                                    n = 100,
                                    colvar = 3:6, nclass = 10, nperm = 100)

################################
# more model validations
################################

# loading the libraries
library(randomForest)
library(precrec)

# extract the raster values for the species points as a dataframe
model_data <- terra::extract(pred_choice$all_19_reduced_r2_VIF_full_range_preds, 
                             PA_pts_sf, df = TRUE, ID = FALSE)

# adding species column to the dataframe
model_data$occ <- as.factor(PA_pts_sf$occ)
head(model_data)

# extract the fold indices from the environmental clustering
folds <- scv$folds_list

# create a data.frame to store the prediction of each fold (record)
test_table <- PA_pts
test_table$preds <- NA

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds_list list (but not folds_ids)
  trainSet <- unlist(folds[[k]][1]) # training set indices; first element
  testSet <- unlist(folds[[k]][2]) # testing set indices; second element
  rf <- randomForest(occ ~ ., model_data[trainSet, ], ntree = 500) # model fitting on training set
  test_table$preds[testSet] <- predict(rf, model_data[testSet, ], type = "prob")[,2] # predict the test set
}

# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = test_table$preds, labels = test_table$occ)
auc(precrec_obj)
autoplot(precrec_obj)


# Using LOO CV

# nndm = nearest neighbour distance matching
scv2 <- blockCV::cv_nndm(
  x = PA_pts_sf,
  column = "occ",
  r = pred_choice$all_19_reduced_r2_VIF_preds,
  size = 271957, # range of spatial autocorrelation
  num_sample = 10000, # number of samples of prediction points
  sampling = "regular", # sampling methods; it can be random as well
  min_train = 0.1, # minimum portion to keep in each train fold
  plot = TRUE
)

scv2$k

blockCV::cv_plot(
  cv = scv2, # cv object
  x = PA_pts_sf, # species spatial data
  num_plots = c(1, 100, 1000) # three of folds to plot
)


######################################
# ecospat package
#####################################

library(ecospat)

predictor_spatrasters_native_range$all_19_reduced_r2_VIF
predictor_spatrasters_entire$all_19_reduced_r2_VIF
predictor_spatrasters_invaded$all_19_reduced_r2_VIF

################################
# NATIVE RANGE DATA
################################

NATIVE_speciesEnv <- base::data.frame(
  raster::extract(predictor_spatrasters_native_range$all_19_reduced_r2_VIF, 
                  cbind(species_native$lon, species_native$lat) )
)

head(NATIVE_speciesEnv)

NATIVE_speciesSwd <- cbind(species_native, NATIVE_speciesEnv)

# Process data 
NATIVE_gps_mega <- as.data.frame(NATIVE_speciesSwd) %>%
  # Keep only the lat/lon columns and key environmental variables
  dplyr::select(
    decimalLatitude = lat,
    decimalLongitude = lon,
    predictor_sets$all_19_reduced_r2_VIF
  ) %>%
  dplyr::mutate(species = "Diaphorina_citri") %>%
  dplyr::select(
    species,
    everything()
  ) %>%
  # Drop rows with NA 
  tidyr::drop_na()
head(NATIVE_gps_mega)

# Reproject CRS 
NATIVE_gpsSpatial <- NATIVE_gps_mega 
coordinates(NATIVE_gpsSpatial) <-  ~ decimalLongitude + decimalLatitude
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
slot(NATIVE_gpsSpatial, "proj4string") <- crs_wgs84

NATIVE_gpsSpatial = as.data.frame(NATIVE_gpsSpatial)
NATIVE_gpsSpatial$region = "native"
head(NATIVE_gpsSpatial)

################################
# INVADED RANGE DATA
################################

INVADED_speciesEnv <- base::data.frame(
  raster::extract(predictor_spatrasters_invaded$all_19_reduced_r2_VIF, 
                  cbind(species_invaded$lon, species_invaded$lat) )
)

head(INVADED_speciesEnv)

INVADED_speciesSwd <- cbind(species_invaded, INVADED_speciesEnv)

# Process data 
INVADED_gps_mega <- as.data.frame(INVADED_speciesSwd) %>%
  # Keep only the lat/lon columns and key environmental variables
  dplyr::select(
    decimalLatitude = lat,
    decimalLongitude = lon,
    predictor_sets$all_19_reduced_r2_VIF
  ) %>%
  dplyr::mutate(species = "Diaphorina_citri") %>%
  dplyr::select(
    species,
    everything()
  ) %>%
  # Drop rows with NA 
  tidyr::drop_na()
head(INVADED_gps_mega)

# Reproject CRS 
INVADED_gpsSpatial <- INVADED_gps_mega 
coordinates(INVADED_gpsSpatial) <-  ~ decimalLongitude + decimalLatitude
crs_wgs84 <- CRS(SRS_string = "EPSG:4326")
slot(INVADED_gpsSpatial, "proj4string") <- crs_wgs84

INVADED_gpsSpatial = as.data.frame(INVADED_gpsSpatial)
INVADED_gpsSpatial$region = "invaded"
head(INVADED_gpsSpatial)

######################

climate_data_current = rbind(NATIVE_gpsSpatial, INVADED_gpsSpatial)
head(climate_data_current)

bio1_summary = Rmisc::summarySE(data = climate_data_current, 
                 measurevar = "wc2.1_2.5m_bio_1", 
                 groupvars = "region")

bio7_summary = Rmisc::summarySE(data = climate_data_current, 
                 measurevar = "wc2.1_2.5m_bio_7", 
                 groupvars = "region")

bio12_summary = Rmisc::summarySE(data = climate_data_current, 
                 measurevar = "wc2.1_2.5m_bio_12", 
                 groupvars = "region")

bio19_summary = Rmisc::summarySE(data = climate_data_current, 
                 measurevar = "wc2.1_2.5m_bio_19", 
                 groupvars = "region")

CI_bio1 = ggplot(data = bio1_summary, aes(x = region, y = wc2.1_2.5m_bio_1)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = wc2.1_2.5m_bio_1 - ci, ymax = wc2.1_2.5m_bio_1 + ci),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  ylim(18, 26) +
  xlab("") +
  ylab(expression(paste("Annual mean temperature [bio 1] (", degree, "C)"))) +
  ggtitle("a)") ;CI_bio1

CI_bio7 = ggplot(data = bio7_summary, aes(x = region, y = wc2.1_2.5m_bio_7)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = wc2.1_2.5m_bio_7 - ci, ymax = wc2.1_2.5m_bio_7 + ci),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  ylim(18, 26) +
  xlab("") +
  ylab(expression(paste("Temperature annual range [bio 7] (", degree, "C)"))) +
  ggtitle("b)") ;CI_bio7

CI_bio12 = ggplot(data = bio12_summary, aes(x = region, y = wc2.1_2.5m_bio_12)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = wc2.1_2.5m_bio_12 - ci, ymax = wc2.1_2.5m_bio_12 + ci),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  ylim(1000, 1800) +
  xlab("") +
  ylab("Annual precipitation [bio 12] (mm)") +
  ggtitle("c)") ;CI_bio12

CI_bio19 = ggplot(data = bio19_summary, aes(x = region, y = wc2.1_2.5m_bio_19)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = wc2.1_2.5m_bio_19 - ci, ymax = wc2.1_2.5m_bio_19 + ci),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  ylim(160, 240) +
  xlab("") +
  ylab("Precipitation of the coldest quarter\n [bio 19] (mm)") +
  ggtitle("d)") ;CI_bio19


CI_gridplots = gridExtra::grid.arrange(CI_bio1, CI_bio7, CI_bio12, CI_bio19)

ggsave("CI_gridplots.svg", CI_gridplots, width = 6, height = 6)

gathered_data <- tidyr::gather(climate_data_current, key = "climate_variable", value = "value", 
                        wc2.1_2.5m_bio_1:wc2.1_2.5m_bio_19)
head(gathered_data)

climate_boxplots = ggplot(data = gathered_data, aes(x = climate_variable, y = value, fill = region)) +
  geom_boxplot(aes(alpha = 0.5)) +
  scale_fill_manual(values = c("red", "forestgreen")) +
  facet_wrap(~climate_variable, scales = "free") +
  theme(legend.position = "right") +
  xlab("") +
  ylab("")

ggsave("climate_boxplots_native_invaded.svg", climate_boxplots, 
       height = 6, width = 7, dpi =600)


climate_df = climate_data_current[4:7]

# Get the current column names
current_names <- colnames(climate_df)

# Create new column names
new_names <- paste0("bio_", c("1", "7", "12", "19"))

# Set the new column names
colnames(climate_df) <- new_names
head(climate_df)


pca_res = prcomp(climate_df, scale. = TRUE)
loadings = pca_res$rotation
loadings

summary(pca_res)
eig.val = factoextra::get_eigenvalue(pca_res)
factoextra::fviz_eig(pca_res, col.var="blue")
var = factoextra::get_pca_var(pca_res)
corrplot(var$cos2, is.corr = FALSE)
factoextra::fviz_cos2(pca_res, choice = "var", axes = 1:2)

factoextra::fviz_pca_var(pca_res,
                         col.var = "cos2", # Color by the quality of representation
                         gradient.cols = c("blue", "gold", "red"),
                         repel = TRUE)

fviz_pca_var_black = factoextra::fviz_pca_var(pca_res,
                         col.var = "black",
                         repel = TRUE)

fviz_pca_var = factoextra::fviz_pca_var(pca_res,
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("blue", "gold", "red"),
                         repel = TRUE) +
                         theme_classic(); fviz_pca_var

ggsave("pca_biplot.png", fviz_pca_var, width = 6, height = 6, dpi = 450)
ggsave("pca_biplot.svg", fviz_pca_var, width = 6, height = 6)

contrib_plot = factoextra::fviz_pca(pca_res, label = "var", col.var = "black", 
                                    habillage = climate_data_current$region,
                     palette = c("red", "black"),
                     addEllipses = TRUE) +
  scale_shape_manual(values = c(17,16)) +
  theme_classic() ;contrib_plot

ggsave("contrib_plot.png", contrib_plot, width = 8, height = 6, dpi = 450)

# Contributions of variables to PC1
a<-factoextra::fviz_contrib(pca_res, choice = "var", axes = 1, fill = "grey", color = "black") +
  #ylim(values = c(0,60)) +
  scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, by = 10)) +
  xlab("Predictor variable") +
  theme_classic() ;a

# Contributions of variables to PC2
b<-factoextra::fviz_contrib(pca_res, choice = "var", axes = 2, fill = "grey", color = "black") +
  #ylim(values = c(0,60)) +
  scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, by = 10)) +
  xlab("Predictor variable") +
  theme_classic() ;b

contribution_pca = grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')
ggsave("contrib_bars.png", contribution_pca, width = 8, height = 6, dpi = 450)

pca_res_dataframe = data.frame("region" = climate_data_current$region, pca_res$x[,1:2])
pca_res_dataframe_3 = data.frame("region" = climate_data_current$region, pca_res$x[,1:3])

clim_pca_plot = ggplot2::ggplot(data = pca_res_dataframe, aes(x = PC1, y = PC2, col = region)) +
  geom_point() + 
  scale_color_manual(values = c("red", "forestgreen")) +
  theme_classic() ;clim_pca_plot

fviz_pca_var + clim_pca_plot


clim_pca_plot = clim_pca_plot + 
  stat_density2d(aes(fill = stat(level)), geom = "polygon", alpha=0.3) +
  scale_fill_gradient2(low = "white", high = "red") ;clim_pca_plot

ggplot_build(clim_pca_plot)

ggplot2::ggplot(data = pca_res_dataframe) +
  geom_density_2d(aes(x = PC1, y = PC2, colour = region)) +
  scale_colour_manual(values = c("red", "grey60")) +
  theme_classic()


clim_pca_plot_2 = ggplot2::ggplot(data = pca_res_dataframe, aes(x = PC1, y = PC2, col = region)) +
  geom_point() + 
  scale_color_manual(values = c("red", "forestgreen")) +
  #scale_fill_manual(values = c("red", "forestgreen")) +
  ggConvexHull::geom_convexhull(aes(x = PC1, y = PC2, fill = region, colour = region), alpha = 0.08) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth = 0.8) +
  geom_vline(xintercept=0, linetype="dashed", colour= "grey", linewidth = 0.8) 
  
clim_pca_plot_2

# Extract convex hull data from the plot
convex_hull_data <- ggplot_build(clim_pca_plot_2)$data[[2]]

# Split the data into two groups (invaded and native)
invaded_data <- subset(convex_hull_data, group == 1)
native_data <- subset(convex_hull_data, group == 2)

# Convert the convex hull data to SpatialPolygons
invaded_poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(invaded_data$x, invaded_data$y))), ID = "invaded")))                                                             
plot(invaded_poly)
native_poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(native_data$x, native_data$y))), ID = "native")))
plot(native_poly)

overlap_poly <- rgeos::gIntersection(invaded_poly, native_poly)
plot(overlap_poly)
overlap_area <- gArea(overlap_poly)

invaded_area <- gArea(invaded_poly)
native_area <- gArea(native_poly)

# Calculate total area
total_area <- invaded_area + native_area - overlap_area
total_area

overlap_percentage = overlap_area/total_area *100
overlap_percentage

invaded_expanded = (invaded_area - overlap_area) / total_area *100
invaded_expanded

native_expanded = (native_area - overlap_area) / total_area *100
native_expanded

plot(invaded_poly)
plot(native_poly, add = TRUE)
plot(overlap_poly, add=TRUE, col = "red")

# Convert the polygons to data frames for ggplot2
overlap_df <- fortify(overlap_poly)
native_df <- fortify(native_poly)
invaded_df <- fortify(invaded_poly)

# Create a ggplot2 plot
library(ggplot2)
  
niche_polygons = ggplot2::ggplot() +
  geom_point(data = pca_res_dataframe, aes(x = PC1, y = PC2, col = region, shape = region)) + 
  scale_color_manual(values = c("red", "black")) +
  scale_shape_manual(values=c(17, 16))+
  geom_polygon(data = native_df, aes(x = long, y = lat, group = group), fill = "black", alpha = 0.15) +
  geom_polygon(data = invaded_df, aes(x = long, y = lat, group = group), fill = "red", alpha = 0.15) +
  geom_polygon(data = overlap_df, aes(x = long, y = lat, group = group), fill = "grey60", col = "black", alpha = 0.3) +
  scale_x_continuous(limits = c(-8,8), breaks = seq(-8, 8, by = 2)) +
  scale_y_continuous(limits = c(-4,4), breaks = seq(-4, 4, by = 2)) +
  ggtitle("Niche overlap: native vs invaded range") +
  xlab("PC 1 (50.5%)") +
  ylab("PC 2 (26.8%)") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 0.3) +
  geom_vline(xintercept=0, linetype="dashed", colour= "black", linewidth = 0.3) +
  theme_classic() ;niche_polygons

ggsave("niche_polygons.png", niche_polygons, width = 8, height = 6, dpi = 450)
ggsave("niche_polygons.svg", niche_polygons, width = 8, height = 6)

library(rgl)
cls = c("red", "forestgreen")
rgl::plot3d(pca_res_dataframe_3, col = cls[as.factor(pca_res_dataframe_3$region)],
            box=F, axes=T, type = "p")
?plot3d

#############################

all.scores = dplyr::select(pca_res_dataframe, c("PC1", "PC2"))
head(all.scores)

native.scores = dplyr::filter(pca_res_dataframe, region == "native") %>%
  dplyr::select(., c("PC1", "PC2"))

invaded.scores =dplyr::filter(pca_res_dataframe, region == "invaded") %>%
  dplyr::select(., c("PC1", "PC2"))
head(invaded.scores)

# BELOW NOT QUITE WORKING YET
# # get PCA scores for background points
# 
# bg.pts.entire = background_points$background_points_all_19_reduced_r2_VIF[,4:7]
# bg.pts.invaded = background_points_invaded_range$background_points_all_19_reduced_r2_VIF[,4:7]
# bg.pts.native = background_points_native_range$background_points_all_19_reduced_r2_VIF[,4:7]
# 
# bg.pts.entire.pca = prcomp(bg.pts.entire, scale. = TRUE)
# bg.pts.invaded.pca = prcomp(bg.pts.invaded, scale. = TRUE)
# bg.pts.native.pca = prcomp(bg.pts.native, scale. = TRUE)
# 
# bg.pts.entire.loadings = as.data.frame( bg.pts.entire.pca$x[,1:2] )
# head(bg.pts.entire.loadings)
# bg.pts.invaded.loadings = as.data.frame( bg.pts.invaded.pca$x[,1:2] )
# bg.pts.native.loadings = as.data.frame( bg.pts.native.pca$x[,1:2] )
# head(bg.pts.native.loadings)
# 
# nativeGrid = ecospat::ecospat.grid.clim.dyn(bg.pts.entire.loadings,
#                                             bg.pts.native.loadings,
#                                             native.scores)
# 
# invadedGrid = ecospat::ecospat.grid.clim.dyn(bg.pts.entire.loadings,
#                                             bg.pts.invaded.loadings,
#                                             invaded.scores)
# 
# ecospat.plot.niche.dyn(nativeGrid, invasiveGrid, quant = 0.1, interest = 2, title = "Niche Overlap", 
#                        name.axis1 = "PC1", name.axis2 = "PC2")
