# File where we generate the final models based on all the data -----------

all_data <- fread("Output/ModelBuilding/all_labelled_feat_vdba.csv")

for (target in target_activities){
  
  # load in the optimal hyperparameters
  parameters <- fread(file.path("ModelBuilding", paste0(target, "_HP_Optimisation.csv")))
  
  # set all the data to binary
  other2 <- all_data %>%
    mutate(activity = ifelse(activity == target, target, "Other"))
  
  # Feature selection
  best_features <- select_features(other2)
  
  # Subset to best predictors
  training_data <- other2 %>%
    select(c(!!!syms(best_features), "activity")) %>%
    na.omit()
  
  # Class weights
  class_freq <- table(training_data$activity)
  
  class_weights <- 1 / class_freq
  class_weights <- class_weights / sum(class_weights)
  
  training_data$activity <- as.factor(training_data$activity)
  
  # Train SVM
  SVM_model <- svm(
    activity ~ ., 
    data = training_data,
    type = "C-classification",
    kernel = parameters$best_kernel,
    cost   = parameters$best_cost,
    gamma  = parameters$best_gamma,
    class.weights = class_weights
  )
  
  # save this model as an RDS object
  saveRDS(SVM_model, file = file.path("Output/ModelBuilding", paste0(target, "_SVM.RDS")))
}
