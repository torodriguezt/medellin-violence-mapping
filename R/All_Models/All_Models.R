################################################################################
######            Fitting every model of the article (repo root)           ######
################################################################################
# Each script reads results/data/ and saves its INLA fit to results/fits/.

rm(list = ls()); source("R/All_Models/model_M0.R")         # separable baseline
rm(list = ls()); source("R/All_Models/model_M1.R")         # static shared components
rm(list = ls()); source("R/All_Models/model_M2_I.R")       # + Type I interaction
rm(list = ls()); source("R/All_Models/model_M2_III.R")     # + Type III interaction
rm(list = ls()); source("R/All_Models/model_M3_annual.R")  # delta_t by year (l = T)
rm(list = ls()); source("R/All_Models/model_M3_l3.R")      # delta_t by period (reported)
rm(list = ls()); source("R/All_Models/model_SCM.R")        # shared interaction rho_t
rm(list = ls()); source("R/All_Models/model_M5.R")         # delta_t + rho_t (exploratory)

## Types II and IV take about an hour each and end with numerical warnings;
## they enter no table. Uncomment to refit them.
# rm(list = ls()); source("R/All_Models/model_M2_II.R")
# rm(list = ls()); source("R/All_Models/model_M2_IV.R")
