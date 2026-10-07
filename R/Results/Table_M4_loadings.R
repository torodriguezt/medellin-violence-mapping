################################################################################
# Article table: M4 spatial loadings, their sexual/non-sexual ratio and all
# period contrasts. Summaries use 5,000 joint hyperparameter draws (seed 511010).
# The non-sexual loading in 2018-2019 is fixed at one.
################################################################################
suppressPackageStartupMessages(library(INLA))
OUT <- "results/tables/M4"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

fit <- readRDS("results/fits/M3_l3_both_loadings_all.rds")
BETA <- paste("Beta for", c("idx_psi1_conf", "idx_psi1_post",
                            "idx_psi2_pre", "idx_psi2_conf", "idx_psi2_post"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(511010)
hs <- inla.hyperpar.sample(5000, fit, intern = FALSE)
ns <- cbind(pre = 1, res = hs[, BETA[1]], post = hs[, BETA[2]])
sx <- hs[, BETA[3:5], drop = FALSE]
colnames(sx) <- colnames(ns)
ratio <- sx / ns

period_summary <- function(x, prefix) {
  contrasts <- cbind(res_pre = x[, "res"] - x[, "pre"],
                     post_res = x[, "post"] - x[, "res"],
                     post_pre = x[, "post"] - x[, "pre"])
  draws <- cbind(x, contrasts)
  data.frame(quantity = paste(prefix, colnames(draws), sep = "_"),
             mean = colMeans(draws),
             q025 = apply(draws, 2, quantile, 0.025),
             q975 = apply(draws, 2, quantile, 0.975),
             P_gt_0 = c(rep(NA_real_, 3), colMeans(contrasts > 0)), row.names = NULL)
}
tab <- rbind(period_summary(ns, "ns"), period_summary(sx, "sx"),
             period_summary(ratio, "ratio"))
write.csv(tab, file.path(OUT, "period_loadings.csv"), row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)
