################################################################################
# Notifier sensitivity: notifications from health institutions only, without
# the family commissaries (linked to the raw MEData export in 2_cases.R), with
# age-standardised expected counts rebuilt from them. Fits M4 (period loadings
# in both outcomes) from initial loadings of 0, 1 and -1, keeping the fit with
# the highest marginal likelihood, M3 (l = 3), and M4 by perpetrator
# relationship with crude expected counts as in the stratified analysis.
################################################################################
source("R/All_Models/model_data.R")
stopifnot("Run data preparation with the raw MEData export first" = all(!is.na(dat$E_health)))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

for (p in c("pre", "conf", "post"))
  dat[[paste0("idx_psi1_", p)]] <- ifelse(dat$resp == 1 & dat$period == p, dat$id_area, NA)

LOAD <- list(beta = list(prior = "normal", param = c(0, 1), fixed = FALSE, initial = 0))
m4 <- y ~ 0 + resp_f +
  f(idx_psi1_pre, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi1_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = LOAD) +
  f(idx_psi1_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = LOAD) +
  f(idx_psi2_pre,  copy = "idx_psi1_pre", fixed = FALSE, hyper = LOAD) +
  f(idx_psi2_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = LOAD) +
  f(idx_psi2_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = LOAD) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

m3 <- y ~ 0 + resp_f +
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

crude <- function(d) d$pop * ave(d$y, d$resp, FUN = sum) / ave(d$pop, d$resp, FUN = sum)
variants <- list(
  health               = function(d) { d$y <- d$y_health; d$E <- d$E_health; d },
  health_intrafamilial = function(d) { d$y <- d$y_health_intrafamilial; d$E <- crude(d); d },
  health_complement    = function(d) { d$y <- d$y_health - d$y_health_intrafamilial
                                        d$E <- crude(d); d })

summ <- function(x, q) data.frame(quantity = q, mean = mean(x), q025 = quantile(x, 0.025),
                                  q975 = quantile(x, 0.975), P_gt_0 = mean(x > 0), row.names = NULL)
beta1 <- paste0("Beta for idx_psi1_", c("conf", "post"))
beta2 <- paste0("Beta for idx_psi2_", c("pre", "conf", "post"))

rows <- starts <- list()
for (v in names(variants)) {
  d <- variants[[v]](dat)
  cat(sprintf("\n== %s: %d non-sexual and %d sexual notifications\n", v,
              sum(d$y[d$resp == 1]), sum(d$y[d$resp == 2])))

  ## M4 from three starting values; keep the highest marginal likelihood
  best <- NULL
  for (init in c(0, 1, -1)) {
    LOAD$beta$initial <- init
    fit <- inla(m4, family = "poisson", data = d, E = E,
                control.predictor = list(compute = TRUE),
                control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
                control.mode = list(restart = FALSE))
    load_means <- fit$summary.hyperpar[c(beta1, beta2), "mean"]
    starts[[paste(v, init)]] <- data.frame(variant = v, initial = init, mlik = fit$mlik[1, 1],
                                           WAIC = fit$waic$waic, negative_loadings = any(load_means < 0))
    cat(sprintf("   M4 start %2d: mlik %.2f, WAIC %.1f, loadings %s\n", init, fit$mlik[1, 1],
                fit$waic$waic, paste(sprintf("%.2f", load_means), collapse = " ")))
    if (is.null(best) || fit$mlik[1, 1] > best$mlik[1, 1]) best <- fit
  }
  if (any(best$summary.hyperpar[c(beta1, beta2), "mean"] < 0))
    warning(v, ": the best M4 mode has negative loadings; do not report it")
  saveRDS(best, file.path("results/fits", paste0("M3_l3_both_loadings_", v, ".rds")))

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(511013)
  hs <- inla.hyperpar.sample(5000, best, intern = FALSE)
  ns <- cbind(1, hs[, beta1]); sx <- hs[, beta2]; r <- sx / ns
  rows[[v]] <- cbind(variant = v, model = "M4", rbind(
    summ(ns[, 2], "ns_res"), summ(ns[, 3], "ns_post"),
    summ(sx[, 1], "sx_pre"), summ(sx[, 2], "sx_res"), summ(sx[, 3], "sx_post"),
    summ(ns[, 2] - 1, "ns_res_minus_pre"), summ(sx[, 2] - sx[, 1], "sx_res_minus_pre"),
    summ(r[, 2] - r[, 1], "ratio_res_minus_pre"),
    summ(ns[, 3] - ns[, 2], "ns_post_minus_res"), summ(sx[, 3] - sx[, 2], "sx_post_minus_res"),
    summ(ns[, 3] - 1, "ns_post_minus_pre"), summ(sx[, 3] - sx[, 1], "sx_post_minus_pre"),
    summ(r[, 3] - r[, 1], "ratio_post_minus_pre")))

  ## M3 (l = 3) for all health notifications
  if (v == "health") {
    fit <- fit_model(m3, "M3_l3_health", data = d)
    set.seed(511014)
    hs <- inla.hyperpar.sample(5000, fit, intern = FALSE)
    rows[["M3"]] <- cbind(variant = v, model = "M3", rbind(
      summ(hs[, beta2[2]] - hs[, beta2[1]], "D_res_minus_pre"),
      summ(hs[, beta2[3]] - hs[, beta2[2]], "post_minus_res")))
  }
}
tab <- do.call(rbind, rows)
write.csv(tab, "results/tables/notifier_health_only.csv", row.names = FALSE)
write.csv(do.call(rbind, starts), "results/tables/notifier_health_only_starts.csv", row.names = FALSE)
cat("\n")
print(format(tab, digits = 3), row.names = FALSE)
