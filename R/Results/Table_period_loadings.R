################################################################################
# Table: relative spatial loadings by period and their pairwise contrasts in
# M3 (l = 3), all from 5,000 joint hyperparameter draws (seed 2026).
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

fit <- readRDS("results/fits/M3_l3.rds")
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026)
hs <- inla.hyperpar.sample(5000, fit, intern = FALSE)

d <- hs[, paste0("Beta for idx_psi2_", c("pre", "conf", "post"))]
colnames(d) <- c("pre", "lock", "post")
draws <- cbind(d, lock_minus_pre  = d[, "lock"] - d[, "pre"],
                  post_minus_lock = d[, "post"] - d[, "lock"],
                  post_minus_pre  = d[, "post"] - d[, "pre"])

tab <- data.frame(quantity = colnames(draws),
                  mean = colMeans(draws),
                  q025 = apply(draws, 2, quantile, 0.025),
                  q975 = apply(draws, 2, quantile, 0.975),
                  P_gt_0 = colMeans(draws > 0), row.names = NULL)

write.csv(tab, "results/tables/period_loadings.csv", row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)
