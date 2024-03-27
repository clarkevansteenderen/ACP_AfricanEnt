all_19 = c(
  "wc2.1_2.5m_bio_1",
  "wc2.1_2.5m_bio_2",
  "wc2.1_2.5m_bio_3",
  "wc2.1_2.5m_bio_4",
  "wc2.1_2.5m_bio_5",
  "wc2.1_2.5m_bio_6",
  "wc2.1_2.5m_bio_7",
  "wc2.1_2.5m_bio_8",
  "wc2.1_2.5m_bio_9",
  "wc2.1_2.5m_bio_10",
  "wc2.1_2.5m_bio_11",
  "wc2.1_2.5m_bio_12",
  "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14",
  "wc2.1_2.5m_bio_15",
  "wc2.1_2.5m_bio_16",
  "wc2.1_2.5m_bio_17",
  "wc2.1_2.5m_bio_18",
  "wc2.1_2.5m_bio_19"
)

# all_19_reduced_r2 = c(
#   "wc2.1_2.5m_bio_1",
#   "wc2.1_2.5m_bio_5",
#   "wc2.1_2.5m_bio_7",
#   "wc2.1_2.5m_bio_12",
#   "wc2.1_2.5m_bio_14"
# )

all_19_reduced_r2_VIF = c(
  "wc2.1_2.5m_bio_1",
  "wc2.1_2.5m_bio_7",
  "wc2.1_2.5m_bio_12",
  "wc2.1_2.5m_bio_19"
)

predictor_sets = list(
  all_19 = all_19,
  all_19_reduced_r2_VIF = all_19_reduced_r2_VIF
)

############################################################
# predictors in the native distribution range:
############################################################

# Iterate over predictor_sets list and create spatrasters
# note that we are using the native range predictor subset
for (set_name in names(predictor_sets)) {
  subset_native <- terra::subset(x = predictors_native_range, subset = predictor_sets[[set_name]])
  assign(paste0(set_name, "_native"), subset_native)
}

predictor_spatrasters_native_range = list(
  all_19 = all_19_native,
  all_19_reduced_r2_VIF = all_19_reduced_r2_VIF_native
)

############################################################
# predictors in the invaded distribution range:
############################################################

# Iterate over predictor_sets list and create spatrasters

for (set_name in names(predictor_sets)) {
  subset_invaded <- terra::subset(x = predictors_invaded_range, subset = predictor_sets[[set_name]])
  assign(paste0(set_name, "_invaded"), subset_invaded)
}

predictor_spatrasters_invaded = list(
  all_19 = all_19_invaded,
  all_19_reduced_r2_VIF = all_19_reduced_r2_VIF_invaded
)

############################################################
# predictors in the entire distribution range:
############################################################

# Iterate over predictor_sets list and create spatrasters

for (set_name in names(predictor_sets)) {
  subset_entire <- terra::subset(x = predictors, subset = predictor_sets[[set_name]])
  assign(paste0(set_name, "_entire"), subset_entire)
}

predictor_spatrasters_entire = list(
  all_19 = all_19_entire,
  all_19_reduced_r2_VIF = all_19_reduced_r2_VIF_entire
)


predictor_spatrasters_entire$all_19_reduced_r2_VIF
predictor_spatrasters_native_range$all_19_reduced_r2_VIF
predictor_spatrasters_invaded$all_19_reduced_r2_VIF
