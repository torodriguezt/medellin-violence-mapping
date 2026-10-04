################################################################################
# Tables: lockdown contrast D = delta_lock - delta_pre of M3 (l = 3) under the
# prior on the loadings, the spatial prior, and by perpetrator relationship.
# Loadings are INLA marginal means; D uses 5,000 joint hyperparameter draws
# (seeds of the published results).
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

fits  <- c(main = "M3_l3", N04 = "M3_l3_prior_N04", N11 = "M3_l3_prior_N11",
           ICAR = "M3_l3_ICAR", intrafamilial = "M3_l3_intrafamilial",
           complement = "M3_l3_complement")
seeds <- c(main = 2026, N04 = 511002, N11 = 511003, ICAR = 511004,
           intrafamilial = 511005, complement = 511006)
beta <- paste0("Beta for idx_psi2_", c("pre", "conf", "post"))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
rows <- lapply(names(fits), function(m) {
  fit <- readRDS(file.path("results/fits", paste0(fits[[m]], ".rds")))
  set.seed(seeds[[m]])
  hs <- inla.hyperpar.sample(5000, fit, intern = FALSE)
  D <- hs[, beta[2]] - hs[, beta[1]]
  data.frame(analysis = m,
             delta_pre  = fit$summary.hyperpar[beta[1], "mean"],
             delta_lock = fit$summary.hyperpar[beta[2], "mean"],
             delta_post = fit$summary.hyperpar[beta[3], "mean"],
             D = mean(D), D_q025 = quantile(D, 0.025), D_q975 = quantile(D, 0.975),
             P_D_gt_0 = mean(D > 0), row.names = NULL)
})
tab <- do.call(rbind, rows)

write.csv(tab, "results/tables/sensitivity.csv", row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)
