################################################################################
# Reproduces the analysis of the article, from the input data to the tables
# and figures. Run from the repository root:  source("main.R")
# About 30 minutes with R 4.4.2 and INLA 24.12.11.
################################################################################

source("R/Data_preparation/Data_preparation.R")   # -> results/data/
source("R/All_Models/All_Models.R")               # -> results/fits/
source("R/Robustness/Robustness.R")               # -> results/fits/, results/tables/
source("R/Results/Results.R")                     # -> results/tables/, figures/
