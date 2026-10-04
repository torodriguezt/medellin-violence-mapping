################################################################################
######                 Data preparation (run from repo root)              ######
################################################################################
# Requires the MEData case file in R/Data/Cases/ (see R/README.md).
# Writes the modelling panel, graph and area index to results/data/.

rm(list = ls()); source("R/Data_preparation/1_population.R")
rm(list = ls()); source("R/Data_preparation/2_cases.R")
rm(list = ls()); source("R/Data_preparation/3_merge_units.R")
rm(list = ls()); source("R/Data_preparation/4_expected_counts.R")
rm(list = ls()); source("R/Data_preparation/5_neighbourhood_graph.R")
