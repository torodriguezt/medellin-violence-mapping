################################################################################
# 2,000 joint posterior draws from M3 (l = 3): log relative risks of every
# cell and the intercepts and temporal effects. Gaussian conditional draws
# (skew.corr = FALSE) keep the linear constraints of the interaction exact.
# Used by Table_hotspots.R, Figures_loadings.R and Figures_checks.R.
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/posterior", recursive = TRUE, showWarnings = FALSE)

fit <- readRDS("results/fits/M3_l3.rds")
n <- nrow(fit$summary.linear.predictor)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026)
S <- inla.posterior.sample(2000, fit, seed = 532026, num.threads = "1:1",
                           parallel.configs = FALSE, use.improved.mean = TRUE,
                           skew.corr = FALSE,
                           selection = list(Predictor = 1:n, resp_fnon_sexual = 1,
                                            resp_fsexual = 1, idx_gam_1 = 1:5,
                                            idx_gam_2 = 1:5, idx_dev_2 = 1:5))
latent <- sapply(S, function(s) s$latent[, 1])
rownames(latent) <- rownames(S[[1]]$latent)
is_eta <- grepl("^Predictor:", rownames(latent))

saveRDS(list(eta = latent[is_eta, ], other = latent[!is_eta, ]),
        "results/posterior/M3_l3_draws.rds")
message("Saved 2,000 joint draws of M3 (l = 3)")
