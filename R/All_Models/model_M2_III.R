################################################################################
# M2-III: M1 plus a Type III space-time interaction for each outcome: an ICAR
# field replicated independently by year, each summing to zero over areas.
################################################################################
source("R/All_Models/model_data.R")

formula <- y ~ 0 + resp_f +
  f(idx_psi_1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi_2, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(ia_1, model = "besag", graph = g, scale.model = TRUE, constr = TRUE,
    rankdef = 1L, diagonal = 0, replicate = iy_1, nrep = 5L, hyper = PC_PREC) +
  f(ia_2, model = "besag", graph = g, scale.model = TRUE, constr = TRUE,
    rankdef = 1L, diagonal = 0, replicate = iy_2, nrep = 5L, hyper = PC_PREC)

fit_model(formula, "M2_III")
