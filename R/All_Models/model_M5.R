################################################################################
# M5 (exploratory): both time-varying loadings in one model, the yearly
# spatial loading delta_t of M3 and the yearly interaction loading of SCM.
################################################################################
source("R/All_Models/model_data.R")
source("R/All_Models/inla_rgeneric_scm_change_typeI.R")

sdunif <- "expression:
  logdens = -log_precision/2;
  return(logdens)"

W <- inla.graph2matrix(g); diag(W) <- 0
SCM <- inla.rgeneric.define(inla.rgeneric.SCM, debug = FALSE, k = n_year, W = W,
                            initial.values = c(4, rep(0, n_year)))
A_sh <- kronecker(diag(2), matrix(1, 1, n_area * n_year))   # zero sum per outcome

formula <- y ~ -1 + alpha1 + alpha2 +
  f(idx_psi_1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi2_t1, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_t2, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_t3, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_t4, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_t5, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(ID_time1, model = "rw1", constr = TRUE, hyper = list(prec = list(prior = sdunif))) +
  f(ID_time2, model = "rw1", constr = TRUE, hyper = list(prec = list(prior = sdunif))) +
  f(ID_area1, model = SCM, constr = FALSE, extraconstr = list(A = A_sh, e = c(0, 0)))

message("Fitting M5 ...")
fit <- inla(formula, family = "poisson", data = dat, E = E,
            control.predictor = list(link = 1, compute = TRUE),
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE),
            control.inla = list(strategy = "simplified.laplace", numint.maxfeval = 100000))
saveRDS(fit, "results/fits/M5.rds")
message(sprintf("  M5: WAIC %.1f, DIC %.1f, %.0f s", fit$waic$waic, fit$dic$dic,
                fit$cpu.used[["Total"]]))
