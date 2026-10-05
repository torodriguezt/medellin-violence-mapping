################################################################################
# Geocoding sensitivity: M3 (l = 3) without the notifications whose
# neighbourhood was assigned by name matching, with age-standardised expected
# counts rebuilt from the remaining notifications. Dual hotspots are recomputed
# with the rule of Table_hotspots.R from 1,000 joint posterior draws.
################################################################################
source("R/All_Models/model_data.R")
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

dat$y <- dat$y_coded
dat$E <- dat$E_coded

formula <- y ~ 0 + resp_f +
  f(idx_psi_1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi2_pre,  copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_conf, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_post, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

fit <- fit_model(formula, "M3_l3_coded_only")

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026)
S <- inla.posterior.sample(1000, fit, selection = list(Predictor = seq_len(nrow(dat))),
                           seed = 532026, num.threads = "1:1", parallel.configs = FALSE,
                           use.improved.mean = TRUE, skew.corr = FALSE)
eta <- sapply(S, function(s) s$latent[, 1])
ns <- which(dat$resp == 1); sx <- which(dat$resp == 2)
lp <- fit$summary.linear.predictor$mean

cells <- data.frame(dat[ns, c("cod_barrio", "nombre_barrio", "nombre_comuna", "year")],
                    RR_ns = exp(lp[ns]), RR_sx = exp(lp[sx]),
                    P_joint = rowMeans(eta[ns, ] > 0 & eta[sx, ] > 0))
persistent <- do.call(rbind, lapply(split(cells, cells$cod_barrio), function(d) {
  if (min(d$P_joint) < 1) return(NULL)
  data.frame(d[1, c("cod_barrio", "nombre_barrio", "nombre_comuna")],
             RR_ns = mean(d$RR_ns), RR_sx = mean(d$RR_sx))
}))
write.csv(cells, "results/tables/joint_exceedance_coded_only.csv", row.names = FALSE)
write.csv(persistent, "results/tables/persistent_hotspots_coded_only.csv", row.names = FALSE)
print(persistent, row.names = FALSE)
print(tapply(cells$P_joint > 0.8, cells$year, sum))
