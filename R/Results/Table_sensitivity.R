################################################################################
# Tables: period contrasts of M3 (l = 3), D = delta_res - delta_pre and
# delta_post - delta_res, under the prior on the loadings, the spatial prior,
# by perpetrator relationship, without the notifications geocoded by name
# matching, for women aged 20 and over, and without neglect.
# Loadings are INLA marginal means; D uses 5,000 joint hyperparameter draws
# (seeds of the published results).
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

fits  <- c(main = "M3_l3", N04 = "M3_l3_prior_N04", N11 = "M3_l3_prior_N11",
           ICAR = "M3_l3_ICAR", intrafamilial = "M3_l3_intrafamilial",
           complement = "M3_l3_complement", coded_only = "M3_l3_coded_only",
           adults = "M3_l3_adults", no_neglect = "M3_l3_no_neglect",
           typeIII = "M3_l3_typeIII")
seeds <- c(main = 2026, N04 = 511002, N11 = 511003, ICAR = 511004,
           intrafamilial = 511005, complement = 511006, coded_only = 511007,
           adults = 511008, no_neglect = 511009, typeIII = 511011)
beta <- paste0("Beta for idx_psi2_", c("pre", "conf", "post"))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
rows <- lapply(names(fits), function(m) {
  fit <- readRDS(file.path("results/fits", paste0(fits[[m]], ".rds")))
  set.seed(seeds[[m]])
  hs <- inla.hyperpar.sample(5000, fit, intern = FALSE)
  D    <- hs[, beta[2]] - hs[, beta[1]]
  post <- hs[, beta[3]] - hs[, beta[2]]
  data.frame(analysis = m,
             delta_pre  = fit$summary.hyperpar[beta[1], "mean"],
             delta_res  = fit$summary.hyperpar[beta[2], "mean"],
             delta_post = fit$summary.hyperpar[beta[3], "mean"],
             D = mean(D), D_q025 = quantile(D, 0.025), D_q975 = quantile(D, 0.975),
             P_D_gt_0 = mean(D > 0),
             post_res = mean(post), post_q025 = quantile(post, 0.025),
             post_q975 = quantile(post, 0.975), P_post_lt_0 = mean(post < 0),
             row.names = NULL)
})
tab <- do.call(rbind, rows)

write.csv(tab, "results/tables/sensitivity.csv", row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)
