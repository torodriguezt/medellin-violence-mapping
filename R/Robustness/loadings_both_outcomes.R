################################################################################
# Loading sensitivity: period-specific loadings of the shared field in both
# equations. The non-sexual loading is fixed at one only in 2018-2019, which
# sets the scale of psi; the sexual/non-sexual ratio of loadings in each period
# separates a change specific to sexual violence from a common steepening.
# Fitted to all notifications, to women aged 20 and over, and without neglect.
################################################################################
source("R/All_Models/model_data.R")
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

for (p in c("pre", "conf", "post"))
  dat[[paste0("idx_psi1_", p)]] <- ifelse(dat$resp == 1 & dat$period == p, dat$id_area, NA)

formula <- y ~ 0 + resp_f +
  f(idx_psi1_pre, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi1_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi1_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_pre,  copy = "idx_psi1_pre", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

variants <- list(all = c("y", "E"), adults = c("y_adult", "E_adult"),
                 no_neglect = c("y_noneglect", "E_noneglect"))

rows <- list()
for (v in names(variants)) {
  d <- dat
  d$y <- dat[[variants[[v]][1]]]
  d$E <- dat[[variants[[v]][2]]]
  fit <- fit_model(formula, paste0("M3_l3_both_loadings_", v), data = d)

  ## non-sexual loadings (pre fixed at 1), sexual loadings and their ratio
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(511010)
  hs <- inla.hyperpar.sample(5000, fit, intern = FALSE)
  ns <- cbind(pre = 1, res = hs[, "Beta for idx_psi1_conf"], post = hs[, "Beta for idx_psi1_post"])
  sx <- hs[, paste0("Beta for idx_psi2_", c("pre", "conf", "post"))]
  ratio <- sx / ns
  draws <- cbind(ns_res = ns[, "res"], ns_post = ns[, "post"],
                 sx_pre = sx[, 1], sx_res = sx[, 2], sx_post = sx[, 3],
                 ratio_pre = ratio[, 1], ratio_res = ratio[, 2], ratio_post = ratio[, 3],
                 ratio_res_minus_pre = ratio[, 2] - ratio[, 1],
                 ratio_post_minus_res = ratio[, 3] - ratio[, 2],
                 ns_res_minus_pre = ns[, "res"] - 1)
  rows[[v]] <- data.frame(variant = v, quantity = colnames(draws), mean = colMeans(draws),
                          q025 = apply(draws, 2, quantile, 0.025),
                          q975 = apply(draws, 2, quantile, 0.975),
                          P_gt_0 = colMeans(draws > 0), row.names = NULL)
}
tab <- do.call(rbind, rows)

write.csv(tab, "results/tables/loadings_both_outcomes.csv", row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)
