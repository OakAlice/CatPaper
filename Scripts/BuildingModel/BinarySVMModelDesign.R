# Design the models for behaviour detection -------------------------------

# this system will use multiple binary SVMs (each specialised to a specific behaviour)
# to detect the target behaviours (in the case of the cat studdy, running and walking)
# model intended to generalise between individuals

all_data <- fread("Output/ModelBuilding/all_labelled_feat_vdba.csv")

# Code --------------------------------------------------------------------
# define the bounds for hyperpatameter tuning
kernel_map <- c("radial", "linear", "polynomial")
bounds <- list(
  gamma  = c(0.001, 1),   # range for gamma
  cost   = c(0.1, 10),    # range for cost
  kernel = c(1L, 3L)      # encoded kernel index
)

# assign data into test folds
unique_IDs <- unique(all_data$ID)
ID_groups <- data.frame(
  ID = unique_IDs,
  group = sample(rep(1:4, length.out = length(unique_IDs)))
)

# make changes to the data that make it approrpiate for these models
all_data <- all_data %>%
  mutate(
    activity_status = if_else(activity_status == "inactive", 0, 1),
    activity_level  = recode(activity_level,
                             "inactive" = 0L,
                             "low"      = 1L,
                             "medium"   = 2L,
                             "high"     = 3L)
  )
# build a walking and running detector
for (target in target_activities) {
  results <- data.frame()  # initialise
  
  for (fold in 1:4) { # match to the number of folds from the test fold assignment
    test_ids <- ID_groups$ID[ID_groups$group == fold]
    other <- all_data %>% filter(!ID %in% test_ids)
    test <- all_data %>% filter(ID %in% test_ids)
    
    # make binary labels
    other2 <- other %>%
      mutate(activity = ifelse(activity == target, target, "Other"))
    test2 <- test %>%
      mutate(activity = ifelse(activity == target, target, "Other"))
    
    if (fold == 1) { 
      # tune hyperparameters only once in the first loop
      # figured that it was overkill to do a full nested cross-validation
      bo_results <- BayesianOptimization(
        FUN = function(kernel, cost, gamma) {
          kernel_choice <- kernel_map[round(kernel)]
          ModelOptimisation(
            target = target,
            feature_data = other2,
            kernel_option = kernel_choice,
            cost_option   = cost,
            gamma_option  = gamma
          )
        },
        bounds = bounds,
        init_points = 2,
        n_iter = 5,
        acq = "ucb",
        kappa = 2.576
      )
      
      # save best
      tuning <- data.frame(
        activity       = target,
        best_kernel    = kernel_map[round(bo_results$Best_Par[["kernel"]],0)],
        best_gamma     = bo_results$Best_Par[["gamma"]],
        best_cost      = bo_results$Best_Par[["cost"]],
        best_performance = bo_results$Best_Value
      )
      fwrite(tuning, file.path("Output/ModelBuilding", paste0(target, "_HP_Optimisation.csv")))
    }
    
    # use tuned params
    kernel_option <- tuning$best_kernel
    cost_option   <- tuning$best_cost
    gamma_option  <- tuning$best_gamma
    
    output <- run_cv_iteration(other2, test2, target,
                               kernel_option, cost_option, gamma_option)
    
    output <- as.data.frame(output)
    
    results <- rbind(results, output)
  }
  
  # save that
  fwrite(results, file.path("ModelBuilding", paste0(target, "_CrossValidation.csv")))
}
