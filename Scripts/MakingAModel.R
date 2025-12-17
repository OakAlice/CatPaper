# Making a behaviour prediction model -------------------------------------

# Preparing the labelled data ---------------------------------------------
# before loading into the model, we need to select the appropriate data
# takes in the labelled data in Data/LabelledData/ from_vis and from_vid
source("Scripts/PreparingLabelledData.R")
# saves cleaned_labelled_data.csv to folder ModelBuilding

# Generate features -------------------------------------------------------
source("Scripts/FeatureGeneration.R")




