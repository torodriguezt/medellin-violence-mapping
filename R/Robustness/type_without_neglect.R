################################################################################
# Type sensitivity: M3 (l = 3) with neglect and abandonment (def_naturaleza 3)
# removed from non-sexual violence, and age-standardised expected counts
# rebuilt from the remaining notifications.
################################################################################
source("R/All_Models/model_data.R")

dat$y <- dat$y_noneglect
dat$E <- dat$E_noneglect

formula <- y ~ 0 + resp_f +
  f(idx_psi_1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi2_pre,  copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_conf, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_post, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

fit_model(formula, "M3_l3_no_neglect")
