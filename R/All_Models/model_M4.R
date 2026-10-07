################################################################################
# M4: period-specific loadings of the shared spatial field in both outcomes.
# The non-sexual loading in 2018-2019 is fixed at one to set the scale.
################################################################################
source("R/All_Models/model_data.R")
source("R/All_Models/model_M4_setup.R")

# Keep the existing fit filename used by the article's tables and figures.
fit_model(m4, "M3_l3_both_loadings_all")
