################################################################################
######        Sensitivity analyses and hold-out check of M3 (l = 3)        ######
################################################################################
# Each script refits M3 (l = 3) with one change and saves it to results/fits/.

rm(list = ls()); source("R/Robustness/prior_N04.R")              # N(0, 4) loadings
rm(list = ls()); source("R/Robustness/prior_N11.R")              # N(1, 1) loadings
rm(list = ls()); source("R/Robustness/spatial_ICAR.R")           # ICAR instead of BYM2
rm(list = ls()); source("R/Robustness/stratum_intrafamilial.R")  # intrafamilial only
rm(list = ls()); source("R/Robustness/stratum_complement.R")     # all other cases
rm(list = ls()); source("R/Robustness/geocoding_coded_only.R")   # without name-matched codes
rm(list = ls()); source("R/Robustness/age_adults_20plus.R")      # women aged 20 and over
rm(list = ls()); source("R/Robustness/type_without_neglect.R")   # non-sexual without neglect
rm(list = ls()); source("R/Robustness/interaction_typeIII.R")    # Type III interaction
rm(list = ls()); source("R/Robustness/loadings_both_outcomes.R") # period loadings in both equations
rm(list = ls()); source("R/Robustness/notifier_health_only.R")   # health institutions only
rm(list = ls()); source("R/Robustness/holdout.R")                # 15% hold-out cells
