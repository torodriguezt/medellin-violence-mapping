################################################################################
# Geocoding sensitivity with M4: without the notifications assigned to a
# neighbourhood by name matching, with age-standardised expected counts rebuilt
# from the remaining notifications. The loadings start at 1 with INLA's restart
# of the mode search; from 0 the fit reaches the mode with negative loadings
# in the original investigation. Loadings and contrasts from 5,000 joint
# hyperparameter draws; dual hotspots and the shared-component risk contrasts
# from 1,000 joint posterior draws.
################################################################################
source("R/All_Models/model_data.R")
source("R/All_Models/model_M4_setup.R")
dir.create("results/tables/M4", recursive = TRUE, showWarnings = FALSE)

d <- m4_data("coded_only")
LOAD_INIT <- rep(1, 5)
fit <- fit_m4(m4, d, restart = TRUE, full = TRUE)
stopifnot(all(fit$summary.hyperpar[BETA, "mean"] > 0))
message(sprintf("M4 coded only: mlik %.2f, WAIC %.1f, loadings %s", fit$mlik[1, 1], fit$waic$waic,
                paste(sprintf("%.3f", fit$summary.hyperpar[BETA, "mean"]), collapse = " ")))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(511010)
cs <- m4_contrasts(inla.hyperpar.sample(5000, fit, intern = FALSE))
summ <- function(x) c(mean = mean(x), q025 = unname(quantile(x, 0.025)),
                      q975 = unname(quantile(x, 0.975)), P_gt_0 = mean(x > 0))

set.seed(2026)
S <- inla.posterior.sample(1000, fit, selection = list(Predictor = seq_len(nrow(d)),
                                                       idx_psi1_pre = 1:285),
                           seed = 532026, num.threads = "1:1", parallel.configs = FALSE,
                           use.improved.mean = TRUE, skew.corr = FALSE)
eta <- sapply(S, function(s) s$latent[paste0("Predictor:", seq_len(nrow(d))), 1])
psi <- sapply(S, function(s) s$latent[paste0("idx_psi1_pre:", 1:285), 1])
L   <- t(sapply(S, function(s) s$hyperpar[BETA]))
g   <- apply(psi, 2, function(p) diff(quantile(p, c(0.1, 0.9), type = 7)))
risk <- list(g = g, ns_res_over_pre = exp((L[, 1] - 1) * g),
             sx_res_over_pre = exp((L[, 4] - L[, 3]) * g),
             ns_post_over_pre = exp((L[, 2] - 1) * g), sx_post_over_pre = exp((L[, 5] - L[, 3]) * g))
tab <- rbind(data.frame(quantity = colnames(cs), t(apply(cs, 2, summ)), row.names = NULL),
             data.frame(quantity = names(risk), t(sapply(risk, summ)), row.names = NULL))
write.csv(tab, "results/tables/M4/coded_only_contrasts.csv", row.names = FALSE)
print(format(tab, digits = 3), row.names = FALSE)

## Joint exceedance used by the article hotspot table and maps
ns <- which(d$resp == 1); sx <- which(d$resp == 2)
lp <- fit$summary.linear.predictor$mean
cells <- data.frame(d[ns, c("cod_barrio", "nombre_barrio", "nombre_comuna", "year")],
                    RR_ns = exp(lp[ns]), RR_sx = exp(lp[sx]),
                    P_joint = rowMeans(eta[ns, ] > 0 & eta[sx, ] > 0), row.names = NULL)
write.csv(cells, "results/tables/M4/joint_exceedance_coded_only.csv", row.names = FALSE)
