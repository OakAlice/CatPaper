# Main --------------------------------------------------------------------
# Main script for the analysis of the cat data

# setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/CatPaper")
setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/CatPaper")

# TODO list ---------------------------------------------------------------
# normalise features in model training and before classification of the unlabelled
# change the way the predictions are combined and smoothed for ecological reasonableness
# pretty plots
# automate the reasonableness plot
# deal with the "other" prediction when the VDBA is doing the roll
# change the normal features code to be the "only if" code... ngl its kinda better


# Set up ------------------------------------------------------------------
pacman::p_load(tidyverse,
               data.table,
               tsfeatures,
               future,
               future.apply,
               ranger,
               e1071,
               caret,
               rBayesianOptimization,
               mgcv,
               patchwork,
               purrr)

source("Scripts/FigureFunctions.R")
source("Scripts/FeaturesFunctions.R")
source("Scripts/BuildingModel/BinarySVMFunctions.R")

window_length <- 1 # in seconds
target_activities <- c("Locomotion") # specific things to find

# Generating data ----------------------------------------------------------

## Preparing the labelled data ---------------------------------------------
# before loading into the model, we need to select the appropriate data
# takes in the labelled data in Data/LabelledData/ from_vis and from_vid
# saves cleaned_labelled_data.csv to folder ModelBuilding
source("Scripts/BuildingModel/PreparingLabelledData.R")

## Generate features -------------------------------------------------------
# takes in the cleaned_labelled_data.csv
# generates timeseries and statistical features for each window of data (vars hardcoded in script)
# saves the feature data as all_labelled_features.csv
source("Scripts/BuildingModel/GeneratingFeaturesLabelled.R")

# Generating behavioural classifications -----------------------------------

## Generating VDBA ---------------------------------------------------------
# takes in the cleaned_labelled_data.csv
# Across each continuous sampling period, calculate rolling VDBA 
# saves the resultant 1/sec means and maxes to labelled_VDBA.csv
source("Scripts/BuildingModel/GeneratingVDBALabelled.R")

## Generating Model -------------------------------------------------------
# validate the optimal model design, test, and train
# takes in the all_labelled_features.csv and returns a trained model + performance stats
source("Scripts/BuildingModel/BehaviouralClassification.R")

# Making predictions on the free-roaming data -----------------------------

## Calculating the VDBA from all the free-roaming data --------------------
# takes in the unlabelled data (masssssive files)
# calculates VDBA and features per 1 second 
# saves to the Output/Predictions folder (incrememtal and total)
source("Scripts/GeneratingPredictions/GeneratingFeaturesUnlabelled.R")

# then use the following script to double check the prediction output
# source("Scripts/GeneratingPredictions/CheckingReasonableness.R")

# Quantifying effect of bib -----------------------------------------------
source("Scripts/UnderstandingEcology/CreateFinalBibData.R")
source("Scripts/UnderstandingEcology/BibAnalysis.R")


# GPS Data ----------------------------------------------------------------
source("Scripts/GPS/PreparingGPS.R")
source("Scripts/GPS/AnalysingGPS.R")



