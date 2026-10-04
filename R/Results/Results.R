################################################################################
######          Tables and figures of the article (repo root)             ######
################################################################################
# Requires the fits of R/All_Models/ and R/Robustness/.
# Tables go to results/tables/ and figures (PNG and PDF) to figures/.

rm(list = ls()); source("R/Results/posterior_draws.R")

rm(list = ls()); source("R/Results/Table_model_comparison.R")
rm(list = ls()); source("R/Results/Table_period_loadings.R")
rm(list = ls()); source("R/Results/Table_annual_loadings.R")
rm(list = ls()); source("R/Results/Table_parameters.R")
rm(list = ls()); source("R/Results/Table_sensitivity.R")
rm(list = ls()); source("R/Results/Table_hotspots.R")

rm(list = ls()); source("R/Results/Figures.R")
