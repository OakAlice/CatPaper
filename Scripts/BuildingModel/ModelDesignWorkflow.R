# Making a behaviour prediction model -------------------------------------

# Preparing the labelled data ---------------------------------------------
# before loading into the model, we need to select the appropriate data
# takes in the labelled data in Data/LabelledData/ from_vis and from_vid
# saves cleaned_labelled_data.csv to folder ModelBuilding
source("Scripts/BuildingModel/PreparingLabelledData.R")

# Generate features -------------------------------------------------------
# takes in the cleaned_labelled_data.csv
# generates timeseries and statistical features for each window of data (vars hardcoded in script)
# saves the feature data as all_labelled_features.csv
source("Scripts/BuildingModel/GeneratingFeaturesLabelled.R")

# Generating VDBA ---------------------------------------------------------
# takes in the cleaned_labelled_data.csv
# Across each continuous sampling period, calculate rolling VDBA 
# saves the resultant 1/sec means and maxes to labelled_VDBA.csv
source("Scripts/BuildingModel/GeneratingVDBALabelled.R")

# Training Model ----------------------------------------------------------
# validate the optimal model design, test, and train
# takes in the all_labelled_features.csv and returns a trained model + performance stats
source("Scripts/BuildingModel/ModelBuilding.R")
