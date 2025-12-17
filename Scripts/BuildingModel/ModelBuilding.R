# Model Building ----------------------------------------------------------

# We are doing a very stripped back analysis for this paper
# rather than a full behavioural profile, we will be detecting only activity levels + locomotion
# Firstly we will categorise behaviour into 4 categories: inactive, low, medium, and high
# We will then detect specific locomotion behaviour from the active data

# Validation protocol will individually stratified cross-validation
# fine-tune hyperparameters on the first fold
# then set hyperparameters as constant and iteratively train/test remainder for robust performance score

data <- fread("ModelBuilding/all_labelled_features.csv")

# Part One: Activity Levels -----------------------------------------------
vdba_data <- fread("ModelBuilding/all_labelled_vdba.csv")
# threshold into the different activity levels

ggplot(vdba_data, aes(x = vedba)) + geom_freqpoly()





# build a binary one-vs all locomotion recogniser 