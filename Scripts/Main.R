# Main --------------------------------------------------------------------
# Main script for the analysis of the cat data

setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/CatPaper")


# Set up ------------------------------------------------------------------
pacman::p_load(tidyverse,
               data.table,
               tsfeatures,
               future)

source("Scripts/FigureFunctions.R")

# Generating a behaviour prediction model ---------------------------------
source("Scripts/BuildingModel/ModelDesignWorkflow.R")

# Making predictions on the free-roaming data -----------------------------


# Quantifying effect of bib -----------------------------------------------


# Making figures for the paper --------------------------------------------



