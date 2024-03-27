######################################################################
# RUN TUNED MAXENT MODELS
######################################################################

option_indices <- seq_along(pred_choice)

# Display the options to the user
cat("Choose a set of predictors:\n")
for (i in option_indices) {
  cat(i, "-", names(pred_choice[i]), "\n")
}

# Prompt the user for the option number
predictor_choice <- readline("Enter the number of your choice: ")

# Retrieve the selected option from the background and presence predictor sets
presList = presence_points[[as.integer(predictor_choice)]]
backList = background_points[[as.integer(predictor_choice)]]

answer = readline("Have you already written your background and presence points to a folder? y or n: ")
write_or_not = answer

if(write_or_not == "n" || write_or_not == "N"){
  
  # Choose a directory to save these presence and background points to
  
  presList_choice = readline("Directory to save your presence points (e.g. LITERATURE/wang_set/gps/): ")
  backList_choice = readline("Directory to save your background points (e.g. LITERATURE/wang_set/background/): ")
  # LITERATURE/wang_set/gps/Diaphorina_citri.csv
  # LITERATURE/wang_set/background/Diaphorina_citri.csv
  
  presList_directory = paste(presList_choice, "diaphorina_citri.csv", sep = "")
  backList_directory = paste(backList_choice, "diaphorina_citri.csv", sep = "")
  
  write.csv(presList, file = presList_directory, quote = FALSE, row.names = FALSE)
  write.csv(backList, file = backList_directory, quote = FALSE, row.names = FALSE)
}# if

if(write_or_not == "y" || write_or_not == "Y"){
  presList_choice <- readline("Directory from which to read your presence points (e.g. LITERATURE/wang_set/gps/): ")
  backList_choice <- readline("Directory from which to your background points (e.g. LITERATURE/wang_set/background/): ")
  
  presList_directory <- paste(presList_choice, "diaphorina_citri.csv", sep = "")
  backList_directory <- paste(backList_choice, "diaphorina_citri.csv", sep = "")
}

# manually create the folders you need for storing the model results
# directory to save to:
model_directory = readline("Directory to save your MaxEnt model (e.g. LITERATURE/wang_set/tuned_maxent_model/): ")
model_directory_input = model_directory

# get model features:
cat("Enter model features separated with a comma (e.g. linear, quadratic, hinge), and press Enter again when done: ")
feat = scan(what = character(), sep = ",")

# get regularisation:
cat("Enter the regularization value (e.g. 1): ")
reg = scan(what = numeric(), sep = ",")

####################################################################
# Run model 
# - This model is using tuned MaxEnt settings
####################################################################

message("RUNNING MAXENT MODEL NOW")

megaSDM::MaxEntModel(
  occlist = presList_directory,
  bglist = backList_directory,
  model_output = here::here(model_directory_input),
  ncores = 2,
  nrep = 4,
  alloutputs = TRUE,
  features = feat,
  regularization = reg
)
