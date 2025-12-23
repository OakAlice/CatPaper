# Making the behavioural classifications ----------------------------------
unlabelled_features <- list.files("Output/Predictions", full.names = TRUE, pattern = "_unlabelled_features.csv")
# x <- unlabelled_features[2]

# for each of the unlabelled files, generate features and then make predictions
lapply(unlabelled_features, function(x){
  
  if (file.exists(file.path("Output/Predictions", paste0(name, "_unlabelled_predictions.csv")))){
    print("file already exists")
    next
  }
  fname <- tools::file_path_sans_ext(basename(x))
  name <- str_split(fname, "_")[[1]][1]
  
  dat <- fread(x)
  
  # firstly, categorise the vdba in the same way as wel did for the labelled data
  dat$activity_status <- ifelse(dat$sd_vedba < 0.01, "inactive", "active")
  
  dat <- dat %>%
    mutate(
      activity_level = case_when(
        activity_status == "inactive"                           ~ "inactive",
        mean_VDBA > 0.4 * max(dat$mean_VDBA, na.rm = TRUE)      ~ "high",
        mean_VDBA > 0.1 *  max(dat$mean_VDBA, na.rm = TRUE)     ~ "medium",
        mean_VDBA > 0.05 * max(dat$mean_VDBA, na.rm = TRUE)     ~ "low",
        TRUE                                                    ~ "Other"
      )
    )
    
  # apply the models we prepared earlier!
  # collect all features ever used
  # have to do this so we can clean the data in the same way for both of the target activites
  all_good_features <- unique(unlist(lapply(target_activities, function(target) {
    SVM_model <- readRDS(file.path("Output/ModelBuilding", paste0(target, "_SVM.RDS")))
    names(SVM_model$x.scale$`scaled:center`)
  })))
  clean_data <- dat %>%
    tidyr::drop_na(!!!syms(all_good_features))
  
  # initialise tags once for the individual
  tags <- clean_data %>%
    select(time, ID, activity_level, activity_status, mean_VDBA, sd_vedba)
  
  for (target in target_activities) {
    
    SVM_model <- readRDS(file.path("Output/ModelBuilding", paste0(target, "_SVM.RDS")))
    good_features <- names(SVM_model$x.scale$`scaled:center`)
    
    num_unlabelled <- clean_data %>%
      select(!!!syms(good_features))
    
    tags[[target]] <- predict(SVM_model, newdata = num_unlabelled)
  }
  
  # combine these predictions to make a final "predictions" column
  # this is very basic code at the moment
  # TODO: Fix this to make it variable target names
  tags <- tags %>%
    mutate(
      prediction = case_when(
        Fast_Locomotion == "Fast_Locomotion" ~ "Fast_Locomotion",
        Locomotion      == "Locomotion"      ~ "Locomotion",
        TRUE                                ~ activity_level
      )
    )

  # save for this individual
  fwrite(tags, file.path("Output/Predictions", paste0(name, "_unlabelled_predictions.csv")))
})
