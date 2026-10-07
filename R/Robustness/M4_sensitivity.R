# M4 sensitivity rows reported in article_public_health.tex (tab:appA_m4_sens).
# One reported specification per row; starting-value investigations are separate.
source("R/All_Models/model_data.R")
source("R/All_Models/model_M4_setup.R")
OUT <- "results/tables/M4"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
stopifnot("Prepare data with the raw MEData export first" = all(is.finite(dat$E_health)))

quantities <- c("ns_res_pre", "sx_res_pre", "ratio_res_pre", "ns_post_pre")
summarise_fit <- function(fit, variant, seed = 511010) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  cs <- m4_contrasts(inla.hyperpar.sample(5000, fit, intern = FALSE))[, quantities]
  data.frame(variant = variant, quantity = quantities, mean = colMeans(cs),
             q025 = apply(cs, 2, quantile, 0.025),
             q975 = apply(cs, 2, quantile, 0.975),
             P_gt_0 = colMeans(cs > 0), row.names = NULL)
}

fit <- readRDS(M4_FIT)
rows <- list(all = summarise_fit(fit, "all"))
rm(fit); invisible(gc())

# Published starting values: 1 for Type III, 0 for the other rows here.
# The coded-only fit is produced once by geocoding_coded_only_M4.R.
variants <- c("adults", "no_neglect", "health", "prior_N04", "prior_N11",
              "ICAR", "typeIII", "intrafamilial", "complement",
              "health_intrafamilial", "health_complement")
for (v in variants) {
  LOAD_PRIOR <- switch(v, prior_N04 = c(0, 0.25), prior_N11 = c(1, 1), c(0, 1))
  LOAD_INIT <- rep(if (v == "typeIII") 1 else 0, 5)
  formula <- switch(v, ICAR = m4_icar, typeIII = m4_typeIII, m4)
  data_variant <- if (v %in% c("prior_N04", "prior_N11", "ICAR", "typeIII")) "all" else v
  d <- m4_data(data_variant)
  if (startsWith(v, "health")) {
    # The reported health-only fits used start 0 and this compute configuration.
    fit <- inla(formula, family = "poisson", data = d, E = E,
                control.predictor = list(compute = TRUE),
                control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
                control.mode = list(restart = FALSE))
  } else {
    fit <- fit_m4(formula, d, full = v %in% c("adults", "no_neglect"))
  }
  if (any(fit$summary.hyperpar[BETA, "mean"] < 0))
    stop(v, ": negative loading mode; review the fit before reporting results")
  rows[[v]] <- summarise_fit(fit, v, if (startsWith(v, "health")) 511013 else 511010)
  rm(fit); invisible(gc())
}

coded <- read.csv(file.path(OUT, "coded_only_contrasts.csv"))
stopifnot(all(quantities %in% coded$quantity))
rows$coded_only <- cbind(variant = "coded_only", coded[match(quantities, coded$quantity), ])
tab <- do.call(rbind, rows[c("all", "adults", "no_neglect", "health", "coded_only",
                            "prior_N04", "prior_N11", "ICAR", "typeIII",
                            "intrafamilial", "complement", "health_intrafamilial",
                            "health_complement")])
write.csv(tab, file.path(OUT, "sensitivity.csv"), row.names = FALSE)
print(tab, digits = 3, row.names = FALSE)
