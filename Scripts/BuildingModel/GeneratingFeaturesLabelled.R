# Generating Features for labelled data -----------------------------------

data <- fread("ModelBuilding/cleaned_labelled_data.csv")

# firstly, find all the continuous stretches of data
data <- detect_breaks(data)

# define all the variables for feature generation
features_type <- c("statistical", "timeseries")
window_length <- 1 # in seconds
sample_rate <- 50 # in Hz
overlap_percent <- 0 # as a %

IDs <- split(data, data$ID)

lapply(IDs, function(x){
  print(ID)
  # split into the continuous data bits and feed each to the function
  sections <- split(x, x$break_id)
  
  # now generate the features for each of those small continuous chunks
  features_list <- lapply(
    sections,
    generateFeatures,
    features_type   = features_type,
    window_length   = window_length,
    sample_rate     = sample_rate,
    overlap_percent = overlap_percent
  )
  feature_data <- bind_rows(features_list)
  
  # write to disk for using later (safer than storing in memory)
  ID <- x$ID[1]
  fwrite(feature_data, file.path("ModelBuilding/TemporaryData", paste0(ID, "_features.csv")))
})


# Write them back together ------------------------------------------------
features_files <- list.files("ModelBuilding/TemporaryData", full.names = TRUE, pattern = "_features.csv")
feature_data <- lapply(features_files, function(x){
  fread(x)
})
feature_data <- rbindlist(feature_data, use.names=TRUE)
# save this
fwrite(feature_data, "ModelBuilding/all_labelled_features.csv")
