################################################################################
######          Tables and figures of the article (repo root)             ######
################################################################################
# Requires All_Models.R and Robustness.R; includes the article's appendices.
# Tables go to results/tables/ and figures (PNG and PDF) to figures/.

rm(list = ls()); source("R/Results/posterior_draws.R")
rm(list = ls()); source("R/Results/Table_hotspots.R")       # M3/M4 agreement in the text
rm(list = ls()); source("R/Results/posterior_draws_M4.R")
rm(list = ls()); source("R/Results/psi_continuous_M4.R")
rm(list = ls()); source("R/Results/Table_M4_loadings.R")
rm(list = ls()); source("R/Results/Table_period_loadings.R")
rm(list = ls()); source("R/Results/Table_annual_loadings.R")
rm(list = ls()); source("R/Results/Results_M4.R")
rm(list = ls()); source("R/Results/Figures.R")
rm(list = ls()); source("R/Results/Figures_M4.R")
