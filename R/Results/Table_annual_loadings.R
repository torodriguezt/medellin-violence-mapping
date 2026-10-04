################################################################################
# Table: annual relative spatial loadings of M3 (l = T). Loadings are INLA
# marginal summaries; contrasts against the pre-pandemic mean
# (delta_2018 + delta_2019) / 2 use 5,000 joint hyperparameter draws (seed 2026).
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

fit <- readRDS("results/fits/M3_annual.rds")
beta <- paste0("Beta for idx_psi2_t", 1:5)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026)
d <- inla.hyperpar.sample(5000, fit, intern = FALSE)[, beta]
contrast <- d - rowMeans(d[, 1:2])

tab <- data.frame(year = 2018:2022,
                  delta = fit$summary.hyperpar[beta, "mean"],
                  delta_q025 = fit$summary.hyperpar[beta, "0.025quant"],
                  delta_q975 = fit$summary.hyperpar[beta, "0.975quant"],
                  contrast = colMeans(contrast),
                  contrast_q025 = apply(contrast, 2, quantile, 0.025),
                  contrast_q975 = apply(contrast, 2, quantile, 0.975),
                  P_gt_pre = colMeans(contrast > 0))
tab[1:2, c("contrast", "contrast_q025", "contrast_q975", "P_gt_pre")] <- NA

## contrasts within the restriction period mentioned in the text
within <- cbind(`2021 - 2020` = d[, 4] - d[, 3], `2022 - 2020` = d[, 5] - d[, 3])
print(apply(within, 2, quantile, c(0.5, 0.025, 0.975)))

write.csv(tab, "results/tables/annual_loadings.csv", row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)
