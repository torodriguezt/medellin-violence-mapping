# M4 analyses reported in article_public_health.tex and its appendices.
# Run after All_Models.R. Tables go to results/tables/M4/ and M4_starts/.

rm(list = ls()); source("R/Robustness/geocoding_coded_only_M4.R") # hotspot sensitivity
rm(list = ls()); source("R/Robustness/M4_sensitivity.R")         # appendix sensitivity table
rm(list = ls()); source("R/Robustness/holdout_M4.R")              # reported predictive validation
rm(list = ls()); source("R/Robustness/M4_starting_values.R")      # appendix identification table
rm(list = ls()); source("R/Robustness/M4_phi2_profile.R")         # remaining identification rows
