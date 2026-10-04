################################################################################
# Spatial-prior sensitivity: M3 (l = 3) with intrinsic CAR (besag) priors in
# place of BYM2 for the shared field psi and the specific fields phi_k.
################################################################################
source("R/All_Models/model_data.R")

formula <- y ~ 0 + resp_f +
  f(idx_psi_1, model = "besag", graph = g, scale.model = TRUE, hyper = PC_PREC) +
  f(idx_psi2_pre,  copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_conf, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_post, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "besag", graph = g, scale.model = TRUE, hyper = PC_PREC) +
  f(idx_phi2, model = "besag", graph = g, scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

fit_model(formula, "M3_l3_ICAR")
