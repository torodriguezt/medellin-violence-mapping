################################################################################
# SCM: shared-interaction model motivated by Retegui, Etxeberria and Ugarte
# (2024). A Type I space-time interaction shared by both outcomes is loaded by
# rho_t and 1/rho_t (rgeneric), so q_t = rho_t^-2 is the relative interaction
# loading; an ICAR field is shared through reciprocal static scalings (besag2).
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
  f(ID_area, model = "besag2", graph = g, scale.model = TRUE, constr = TRUE,
    hyper = list(prec = list(prior = sdunif))) +
  f(ID_unst, model = "iid", hyper = list(prec = list(prior = sdunif))) +
  f(ID_time1, model = "rw1", constr = TRUE, hyper = list(prec = list(prior = sdunif))) +
  f(ID_time2, model = "rw1", constr = TRUE, hyper = list(prec = list(prior = sdunif))) +
  f(ID_area1, model = SCM, constr = FALSE, extraconstr = list(A = A_sh, e = c(0, 0)))

message("Fitting SCM ...")
fit <- inla(formula, family = "poisson", data = dat, E = E,
            control.predictor = list(link = 1, compute = TRUE),
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE,
                                   return.marginals.predictor = TRUE),
            control.inla = list(strategy = "simplified.laplace", numint.maxfeval = 100000))
saveRDS(fit, "results/fits/SCM.rds")
message(sprintf("  SCM: WAIC %.1f, DIC %.1f, %.0f s", fit$waic$waic, fit$dic$dic,
                fit$cpu.used[["Total"]]))
