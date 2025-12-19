# Calculating VDBA for each 1 second of the free-roaming data -------------

# generating all of the features for every 1 second of data takes like a week of processing time
# options to make faster would be to run on a HPC cluster of some kind
# alternatively, only generate the features we need (i.e., the ones that end up being used)
# I have tried to do this before and struggled but this time I'm committing to figuring it out
# firstly, pull only the features we need
all_good_features <- unique(unlist(lapply(target_activities, function(target) {
  SVM_model <- readRDS(file.path("Output/ModelBuilding", paste0(target, "_SVM.RDS")))
  names(SVM_model$x.scale$`scaled:center`)
})))

# identify which of our features we are going to need to call by looking at the ones we have
# remove the axis calls from the specific features
needed_features <- all_good_features[grepl("^[xyz]_", all_good_features)]
needed_features <- unique(sub("^[xyz]_", "", needed_features))

# now determine which of the tsfeatures calls we need to make
# firstly I mapped each of the outputs back to the original inputs
# I did this by manually calling each of the left options and seeing what it created (right)
features_mapping = list(
  "acf_features" = c("x_acf1", "x_acf10", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10"),
  "arch_stat" = c("ARCH.LM"),
  "autocorr_features" = c("embed2_incircle_1", "embed2_incircle_2", "ac_9", "firstmin_ac", "trev_num", "motiftwo_entro3", "walker_propcross"),
  "crossing_points" = c("crossing_points"),
  "dist_features" = c("histogram_mode_10", "outlierinclude_mdrmd"),
  "entropy" = c("entropy"),
  "firstzero_ac" = c("firstzero_ac"),
  "flat_spots" = c("flat_spots"),
  "heterogeneity" = c("arch_acf", "garch_acf", "arch_r2", "garch_r2"),
  "hw_parameters" = c("alpha", "beta", "gamma"),
  "hurst" = c("hurst"),
  "lumpiness" = c("lumpiness"),
  "stability" = c("stability"),
  "max_level_shift" = c("max_level_shift", "time_level_shift"),
  "max_var_shift" = c("max_var_shift", "time_var_shift"),
  "max_kl_shift" = c("max_kl_shift", "time_kl_shift"),
  "nonlinearity" = c("nonlinearity"),
  "pacf_features" = c("x_pacf5", "diff1x_pacf5", "diff2x_pacf5"),
  "pred_features" = c("localsimple_mean1", "localsimple_lfitac", "sampen_first"),
  "scal_features" = c("fluctanal_prop_r1"),
  "station_features" = c("std1st_der", "spreadrandomlocal_meantaul_50", "spreadrandomlocal_meantaul_ac2"),
  "stl_features" = c("nperiods", "seasonal_period", "trend", "spike", "linearity", "curvature", "e_acf1", "e_acf10"),
  "unitroot_kpss" = c("unitroot_kpss"),
  "zero_proportion" = c("zero_proportion")
)

needed_groups <- names(features_mapping)[
  sapply(features_mapping, function(feature_list) any(feature_list %in% needed_features))
]
# therefore, when I go to call "generate features" in the next step, I only have to call those. YAY. Time saved!! Huge!
# now generate them for each chunk

# Process each of the files -----------------------------------------------
unlabelled_files <- list.files("Data/RawData", full.names = TRUE, pattern = ".csv")
# x <- unlabelled_files[2]

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
    # chunk_size <- 400
    n <- nrow(dat)
    starts <- seq(1, n, by = chunk_size)
    
    features_list <- lapply(starts, function(start) {
      if (file.exists(file.path("Output/Predictions/TemporaryData", paste0(name, "_processed_", start, ".csv")))){
        return()
      } else {
        end <- min(start + chunk_size - 1, n)
        data_chunk <- dat[start:end]
        
        # this only generates the specific features as used in the models
        #time_specific <- system.time({
          feature_data_specific <- generateSpecificFeatures(
            data = data_chunk, 
            specific_features = all_good_features, 
            needed_groups = needed_groups,
            window_length = 1, 
            sample_rate = 50, 
            overlap_percent = 0
          )
        # })
        # print(time_specific)
        
        # Original call, with all the features
        # time_full <- system.time({
        #   feature_data_full <- generateFeatures(
        #     data = data_chunk,
        #     features_type = c("statistical", "timeseries"),
        #     window_length = 1,
        #     sample_rate = 50,
        #     overlap_percent = 0
        #   )
        # })
        # print(time_full)
        
        # now generate the VDBA and add that in
        vdba_data <- generate_vdba(data_chunk, window = 1, freq = 50)
        seconds <- vdba_data %>%
          mutate(sample_i = row_number(),
                 second = floor((sample_i - 1) / 50)) %>% ## 50 is the frequency
          group_by(second) %>%
          summarise(
            mean_VDBA = mean(vedba, na.rm = TRUE),
            sd_vedba = sd(vedba, na.rm = TRUE),
            time = first(time),
            .groups = "drop"
          ) %>%
          select(-second)
        
        # add this back to the feature data
        setDT(feature_data_specific)
        setDT(seconds)
        setkey(feature_data_specific, time)
        setkey(seconds, time)
        dat <- seconds[feature_data_specific, roll = "nearest"]
        
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
    features_to_normalise <- colnames(feature_data)[!colnames(feature_data) %in% c("ID", "time", "mean_VDBA", "sd_vedba")]
    feature_data[, (features_to_normalise) := lapply(.SD, function(x) {
      s <- sd(x, na.rm = TRUE)
      if (s == 0 || is.na(s)) return(rep(0, .N))
      (x - mean(x, na.rm = TRUE)) / s
    }), .SDcols = features_to_normalise]
    
    fwrite(feature_data, file.path("Output/Predictions", paste0(name, "_unlabelled_features.csv")))
  }
})
