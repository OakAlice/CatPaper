# Model Building ----------------------------------------------------------

# We are doing a very stripped back analysis for this paper
# rather than a full behavioural profile, we will be detecting only activity levels + locomotion
# Firstly we will categorise behaviour into 4 categories: inactive, low, medium, and high
# We will then detect specific locomotion behaviour from the active data

# Validation protocol will individually stratified cross-validation
# fine-tune hyperparameters on the first fold
# then set hyperparameters as constant and iteratively train/test remainder for robust performance score

feature_data <- fread("ModelBuilding/all_labelled_features.csv")
vdba_data <- fread("ModelBuilding/all_labelled_vdba.csv")

# Part One: Activity Levels -----------------------------------------------
# threshold into the different activity levels

# define thresholds for this research question -- specific to the cat project btw
# inactive is defined as when the sd is the lowest

# ggplot(vdba_data, aes(x = sd_vedba)) + geom_freqpoly()

vdba_data$activity_status <- ifelse(vdba_data$sd_vedba < 0.01, "inactive", "active")

vdba_data <- vdba_data %>%
  mutate(
    activity_level = case_when(
      activity_status == "inactive"                                 ~ "inactive",
      mean_VDBA > 0.4 * max(vdba_data$mean_VDBA, na.rm = TRUE)      ~ "high",
      mean_VDBA > 0.1 *  max(vdba_data$mean_VDBA, na.rm = TRUE)     ~ "medium",
      mean_VDBA > 0.05 * max(vdba_data$mean_VDBA, na.rm = TRUE)     ~ "low",
      TRUE                                                          ~ "Other"
    )
  )


# dat <- vdba_data[1:100000,]
# ggplot(dat, aes(x = seq_len(nrow(dat)), y = vedba, colour = activity_level, group = 1)) +
#   geom_line(linewidth = 0.3)

# combine with the features and save
dat <- merge(feature_data, vdba_data, by = c("ID", "time", "activity"), all.x = TRUE)
fwrite(dat, "ModelBuilding/all_labelled_feat_vdba.csv")

# Locomotion detection ----------------------------------------------------
# from the active data, can we detect locomotion specifically??
# this is a large script so lives in its own file
source("Scripts/BuildingModel/BinarySVMModelDesign.R")


