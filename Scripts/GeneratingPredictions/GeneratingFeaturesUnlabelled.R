# Calculating VDBA for each 1 second of the free-roaming data -------------

# Process each of the files -----------------------------------------------
unlabelled_files <- list.files("Data/RawData", full.names = TRUE, pattern = ".csv")
# x <- unlabelled_files[1]

# for each of the unlabelled files, generate features and then make predictions
lapply(unlabelled_files, function(x){
  fname <- tools::file_path_sans_ext(basename(x))
  name <- gsub("[0-9_]", "", fname)
  
  dat <- fread(x)
  colnames(dat) <- c("time", "x", "y", "z")
  dat$ID <- name
  
  # generate features
  if (file.exists(file.path("Output/Predictions", paste0(name, "_features.csv")))) {
    print(paste("feature data already generated for", name))
  } else {
    
    # chunk it up into increments because
    # 1. it takes so long
    # 2. in case anything goes wrong, its easier to pick up where you left off
    chunk_size <- 100000
    n <- nrow(dat)
    starts <- seq(1, n, by = chunk_size)
    
    features_list <- lapply(starts, function(start) {
      if (file.exists(file.path("Output/Predictions/TemporaryData", paste0(name, "_processed_", start, ".csv")))){
        return()
      } else {
        end <- min(start + chunk_size - 1, n)
        data_chunk <- dat[start:end]
        
        # generate features
        feature_data <- generateFeatures(
          data = data_chunk, 
          features_type = c("statistical", "timeseries"),
          window_length = 1,
          sample_rate = 50,
          overlap_percent = 0
        )
        
        # now generate the VDBA and add that in
        vdba_data <- generate_vdba(data_chunk, window = 1, freq = 50)
        seconds <- vdba_data %>%
          mutate(sample_i = row_number(),
                 second = floor((sample_i - 1) / freq)) %>%
          group_by(second) %>%
          summarise(
            mean_VDBA = mean(vedba, na.rm = TRUE),
            sd_vedba = sd(vedba, na.rm = TRUE),
            time = first(time),
            .groups = "drop"
          ) %>%
          select(-second)
        
        # add this back to the feature data
        setDT(feature_data)
        setDT(seconds)
        setkey(feature_data, time)
        setkey(seconds, time)
        dat <- seconds[feature_data, roll = "nearest"]
        
        # write it as a temp file
        fwrite(dat, file.path("Output/Predictions/TemporaryData", paste0(name, "_processed_", start, ".csv")))
      }
    })
    
    # read back the incremental files 
    incremental_files <- list.files("Output/Predictions/TemporaryData", full.names = TRUE, pattern = name)
    features_list <- lapply(incremental_files, function(x){fread(x)})
    
    # recombine them
    feature_data <- rbindlist(features_list, fill = TRUE)
    
    # normalise to account for the very different sized individuals
    # features_to_normalise <- colnames(feature_data)[!colnames(feature_data) %in% c("ID", "Timestamp")]
    # feature_data[, (features_to_normalise) := lapply(.SD, function(x) {
    #   s <- sd(x, na.rm = TRUE)
    #   if (s == 0 || is.na(s)) return(rep(0, .N))
    #   (x - mean(x, na.rm = TRUE)) / s
    # }), .SDcols = features_to_normalise]
    
    fwrite(feature_data, file.path("Output/Predictions", paste0(name, "_unlabelled_features.csv")))
    # feature_data <- fread(file.path(base_path, "Output", paste0(name, "_Unlabelled_Feature_Data.csv")))
  }
})