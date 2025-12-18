# Anomaly Detection Model Tuning

select_features<- function(data){
  
  top_predictors <- tryCatch({
    
    setDT(data)
    
    # remove bad features
    clean_cols <- removeBadFeatures(data, var_threshold = 0.3, corr_threshold = 0.8)
    clean_feature_data <- data %>%
      select(c(!!!syms(clean_cols), "activity")) %>% 
      na.omit()
    clean_feature_data$activity <- as.factor(clean_feature_data$activity)
    
    # Fit RF on the training data to fidn the best predictor variables
    RF_model <- ranger(
      formula = activity ~ .,
      data = clean_feature_data,
      num.trees = 500,
      mtry = floor(sqrt(ncol(clean_feature_data) - 1)),
      importance = "impurity",
      classification = TRUE
    )
    
    # Extract variable importance and select top 30
    imp <- importance(RF_model)
    top_predictors <- names(sort(imp, decreasing = TRUE))[1:30]
    
  }, error = function(e) {
    message("Random forest failed: ", e$message)
    NULL  # return NULL if it fails
  })
  
  return(top_predictors)
}

# remove redundant and NA columns
removeBadFeatures <- function(data, var_threshold, corr_threshold) {
  
  # Step 1: Calculate variance for numeric columns
  numeric_columns <- data[, .SD, .SDcols = setdiff(names(data), c("activity", "ID", "time"))]
  variances <- numeric_columns[, lapply(.SD, var, na.rm = TRUE)]
  selected_columns <- names(variances)[!is.na(variances) & variances > var_threshold]
  
  # Step 2: Remove highly correlated features
  numeric_columns <- numeric_columns[, ..selected_columns]
  corr_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
  high_corr <- caret::findCorrelation(corr_matrix, cutoff = corr_threshold)
  remaining_features <- setdiff(names(numeric_columns), names(numeric_columns)[high_corr])
  
  return(remaining_features)
}

run_cv_iteration <- function(train, validate, activity, kernel_option, cost_option, gamma_option) {
  message("Kernel: ", kernel_option, " | Cost: ", cost_option, " | Gamma: ", gamma_option)
  
  # Feature selection
  best_features <- select_features(data = train)
  
  # Subset to best predictors
  training_data <- train %>%
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
    kernel = kernel_option,
    cost   = cost_option,
    gamma  = gamma_option,
    class.weights = class_weights
  )
  
  # Validation data
  validation_data <- validate %>%
    select(c(!!!syms(best_features), "activity")) %>%
    na.omit()
  
  num_validate <- validation_data %>% select(c(!!!syms(best_features)))
  ground_truth <- validation_data %>% select(activity)
  
  # Predictions
  predictions <- predict(SVM_model, newdata = num_validate)
  
  # Confusion matrix (padded to match dimensions)
  all_classes <- sort(union(unique(predictions), unique(ground_truth$activity)))
  predicted_classes <- factor(unlist(predictions), levels = all_classes)
  ground_truth_labels <- factor(ground_truth$activity, levels = all_classes)
  
  metrics_table <- NULL
  
  # if there are multiple classes
  if (length(levels(predicted_classes)) > 1 & length(levels(ground_truth_labels)) > 1){
    cm <- confusionMatrix(predicted_classes, ground_truth_labels, positive = activity)
    metrics_table <- data.frame(
      fold       = fold,
      activity   = activity,
      Accuracy   = cm$overall["Accuracy"],
      Kappa      = cm$overall["Kappa"],
      Sensitivity= cm$byClass["Sensitivity"],
      Specificity= cm$byClass["Specificity"],
      Precision  = cm$byClass["Pos Pred Value"],
      Recall     = cm$byClass["Sensitivity"],
      F1         = cm$byClass["F1"],
      BalancedAcc= cm$byClass["Balanced Accuracy"]
    )

  } else { # if there's just one class
    correct <- all(predicted_classes == ground_truth_labels)
    
    metrics_table <- data.frame(
      fold        = fold,
      activity    = activity,
      Accuracy    = ifelse(correct, 1, 0),
      Kappa       = NA,
      Sensitivity = ifelse(correct, 1, 0),
      Specificity = NA,
      Precision   = ifelse(correct, 1, 0),
      Recall      = ifelse(correct, 1, 0),
      F1          = ifelse(correct, 1, 0),
      BalancedAcc = ifelse(correct, 1, 0)
    )
  }
  
  return(metrics_table)
}

# Main optimisation function
ModelOptimisation <- function(target, feature_data, kernel_option, cost_option, gamma_option, k = 3) {
  # Split into folds
  unique_IDs <- unique(feature_data$ID)
  ID_groups <- data.frame(
    ID = unique_IDs,
    group = sample(rep(1:3, length.out = length(unique_IDs)))
  )
  
  # Run CV across k folds
  results <- lapply(1:k, function(i) {
    # new individuals selected as the test for each fold
    test_ids <- ID_groups$ID[ID_groups$group == i]
    other <- feature_data %>% filter(!ID %in% test_ids)
    test <- feature_data %>% filter(ID %in% test_ids)
    
    tryCatch({
      run_cv_iteration(
        train = other,
        validate = test,
        activity = target,
        kernel_option = kernel_option,
        cost_option = cost_option,
        gamma_option = gamma_option
      )
    }, error = function(e) {
      message("Error in CV iteration ", i, ": ", e$message)
      return(NA)
    })
  })
  
  results_table <- rbindlist(results)
  
  f1s <- results_table[,F1]
  
  # Extract F1 scores
  f1 <- mean(f1s, na.rm = TRUE)
  
  # making this not a list with Score and Pred will trigger 
  # Error in This_Score_Pred$Score : $ operator is invalid for atomic vectors
  # because BayesianOptimisation is trying to call the Score object
  return(list(Score = f1, Pred = NA))
}
