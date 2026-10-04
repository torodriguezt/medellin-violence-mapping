################################################################################
# Table: posterior mean and 95% credible interval of every parameter of
# M3 (l = 3). Scales are sigma = tau^(-1/2), integrated over the marginal of
# log tau; the other rows are INLA marginal summaries.
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

fit <- readRDS("results/fits/M3_l3.rds")

marginal_row <- function(tab, name) as.numeric(tab[name, c("mean", "0.025quant", "0.975quant")])
sigma_row <- function(term) {
  m <- fit$internal.marginals.hyperpar[[paste("Log precision for", term)]]
  c(inla.emarginal(function(x) exp(-x / 2), m),
    exp(-inla.qmarginal(c(0.975, 0.025), m) / 2))
}

rows <- list(
  alpha_1     = marginal_row(fit$summary.fixed, "resp_fnon_sexual"),
  alpha_2     = marginal_row(fit$summary.fixed, "resp_fsexual"),
  sigma_psi   = sigma_row("idx_psi_1"),
  pi_psi      = marginal_row(fit$summary.hyperpar, "Phi for idx_psi_1"),
  sigma_phi1  = sigma_row("idx_phi1"),
  pi_phi1     = marginal_row(fit$summary.hyperpar, "Phi for idx_phi1"),
  sigma_phi2  = sigma_row("idx_phi2"),
  pi_phi2     = marginal_row(fit$summary.hyperpar, "Phi for idx_phi2"),
  delta_pre   = marginal_row(fit$summary.hyperpar, "Beta for idx_psi2_pre"),
  delta_lock  = marginal_row(fit$summary.hyperpar, "Beta for idx_psi2_conf"),
  delta_post  = marginal_row(fit$summary.hyperpar, "Beta for idx_psi2_post"),
  lambda      = marginal_row(fit$summary.hyperpar, "Beta for idx_gam_2"),
  sigma_gamma = sigma_row("idx_gam_1"),
  sigma_nu    = sigma_row("idx_dev_2"),
  sigma_Delta1 = sigma_row("icell_1"),
  sigma_Delta2 = sigma_row("icell_2"))

tab <- data.frame(parameter = names(rows), do.call(rbind, rows), row.names = NULL)
names(tab)[2:4] <- c("mean", "q025", "q975")

write.csv(tab, "results/tables/parameters_M3_l3.csv", row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)
