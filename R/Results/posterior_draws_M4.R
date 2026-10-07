################################################################################
# 2,000 joint posterior draws from M4: log relative risks of every cell, the
# intercepts and temporal effects, the shared field psi, the non-sexual field
# phi_1 and the hyperparameters of each draw. Gaussian conditional draws
# (skew.corr = FALSE) keep the linear constraints of the interaction exact.
# Used by Results_M4.R and Figures_M4.R.
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/posterior", recursive = TRUE, showWarnings = FALSE)

fit <- readRDS("results/fits/M3_l3_both_loadings_all.rds")
n <- nrow(fit$summary.linear.predictor)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026)
S <- inla.posterior.sample(2000, fit, seed = 532026, num.threads = "1:1",
                           parallel.configs = FALSE, use.improved.mean = TRUE,
                           skew.corr = FALSE,
                           selection = list(Predictor = 1:n, resp_fnon_sexual = 1,
                                            resp_fsexual = 1, idx_gam_1 = 1:5,
                                            idx_gam_2 = 1:5, idx_dev_2 = 1:5,
                                            idx_psi1_pre = 1:285,
                                            idx_phi1 = 1:285))
latent <- sapply(S, function(s) s$latent[, 1])
rownames(latent) <- rownames(S[[1]]$latent)
is_eta <- grepl("^Predictor:", rownames(latent))
is_psi <- grepl("^idx_psi1_pre:", rownames(latent))
is_phi <- grepl("^idx_phi1:", rownames(latent))

saveRDS(list(eta = latent[is_eta, ], psi = latent[is_psi, ], phi1 = latent[is_phi, ],
             other = latent[!(is_eta | is_psi | is_phi), ],
             hyper = t(sapply(S, function(s) s$hyperpar))),
        "results/posterior/M4_draws.rds")
message("Saved 2,000 joint draws of M4")
