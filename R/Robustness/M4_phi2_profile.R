################################################################################
# M4 along the sexual-specific spatial field phi_2. The level of the sexual
# loadings trades off against phi_2, so M4 is refitted with the scale of phi_2
# fixed at 0.001, 0.05, 0.1, 0.2 and 0.3 (mixing parameter free), and without
# phi_2 from several starting values. Each row gives the log marginal
# likelihood, WAIC, the loadings and the period contrasts (5,000 joint
# hyperparameter draws). Fits are not saved.
################################################################################
source("R/All_Models/model_data.R")
source("R/All_Models/model_M4_setup.R")
dir.create("results/tables/M4_starts", recursive = TRUE, showWarnings = FALSE)

PHI2_HYPER <- PC_BYM2
m4_phi2 <- y ~ 0 + resp_f +
  f(idx_psi1_pre, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi1_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(1)) +
  f(idx_psi1_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(2)) +
  f(idx_psi2_pre,  copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(3)) +
  f(idx_psi2_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(4)) +
  f(idx_psi2_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(5)) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PHI2_HYPER) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)
m4_nophi2 <- y ~ 0 + resp_f +
  f(idx_psi1_pre, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi1_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(1)) +
  f(idx_psi1_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(2)) +
  f(idx_psi2_pre,  copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(3)) +
  f(idx_psi2_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(4)) +
  f(idx_psi2_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(5)) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

runs <- c(lapply(c(0.001, 0.05, 0.1, 0.2, 0.3), function(s)
            list(label = sprintf("sigma_phi2 = %g", s), formula = m4_phi2, sigma = s, init = 0,
                 restart = FALSE)),
          list(list(label = "no phi_2, start 0", formula = m4_nophi2, sigma = NA, init = 0, restart = FALSE),
               list(label = "no phi_2, start 1", formula = m4_nophi2, sigma = NA, init = 1, restart = FALSE),
               list(label = "no phi_2, start 1 restart", formula = m4_nophi2, sigma = NA, init = 1,
                    restart = TRUE)))

rows <- list()
for (r in runs) {
  PHI2_HYPER <- if (is.na(r$sigma)) PC_BYM2 else
    list(prec = list(initial = log(1 / r$sigma^2), fixed = TRUE),
         phi  = list(prior = "pc", param = c(0.5, 2/3)))
  LOAD_INIT <- rep(r$init, 5)
  fit <- fit_m4(r$formula, dat, restart = r$restart)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(511010)
  cs <- m4_contrasts(inla.hyperpar.sample(5000, fit, intern = FALSE))
  q  <- apply(cs, 2, quantile, c(0.025, 0.975))
  load_means <- fit$summary.hyperpar[BETA, "mean"]
  rows[[r$label]] <- data.frame(
    variant = "phi2_profile", start = r$label, restart = r$restart,
    mlik = fit$mlik[1, 1], WAIC = fit$waic$waic, DIC = fit$dic$dic,
    t(setNames(load_means, c("ns_res", "ns_post", "sx_pre", "sx_res", "sx_post"))),
    sigma_phi2 = r$sigma,
    t(setNames(colMeans(cs), colnames(cs))),
    t(setNames(q[1, ], paste0(colnames(cs), "_q025"))),
    t(setNames(q[2, ], paste0(colnames(cs), "_q975"))),
    t(setNames(colMeans(cs > 0), paste0(colnames(cs), "_P"))), row.names = NULL)
  message(sprintf("%s: mlik %.2f, WAIC %.1f, loadings %s", r$label, fit$mlik[1, 1], fit$waic$waic,
                  paste(sprintf("%.3f", load_means), collapse = " ")))
  write.csv(do.call(rbind, rows), "results/tables/M4_starts/phi2_profile.csv", row.names = FALSE)
  rm(fit); invisible(gc())
}
