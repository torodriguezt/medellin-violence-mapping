################################################################################
######            Fitting every model of the article (repo root)           ######
################################################################################
# Each script reads results/data/ and saves its INLA fit to results/fits/.

rm(list = ls()); source("R/All_Models/model_M0.R")         # separable baseline
rm(list = ls()); source("R/All_Models/model_M1.R")         # static shared components
rm(list = ls()); source("R/All_Models/model_M2_I.R")       # + Type I interaction
rm(list = ls()); source("R/All_Models/model_M2_III.R")     # + Type III interaction
rm(list = ls()); source("R/All_Models/model_M3_annual.R")  # delta_t by year (l = T)
rm(list = ls()); source("R/All_Models/model_M3_l3.R")      # delta_t by period
rm(list = ls()); source("R/All_Models/model_M4.R")         # main model: loadings in both outcomes
rm(list = ls()); source("R/All_Models/model_SCM.R")        # shared interaction rho_t
