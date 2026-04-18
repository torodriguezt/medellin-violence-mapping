# =====================================================
# 06_busqueda_priors.R
# Grid de priors y búsqueda del mejor ajuste
# =====================================================

source("05_ajustar_modelos_basicos.R")

# --- Utilidades para predicción y calibración ---
posterior_predict_inla <- function(fit, E, nsim = 3000L) {
  if (is.null(fit$misc$configs)) {
    stop("Ajusta el modelo con control.compute=list(config=TRUE).")
  }
  S <- INLA::inla.posterior.sample(n = nsim, result = fit, seed = 0)
  idx <- grep("^Predictor", rownames(S[[1]]$latent))
  ETA <- sapply(S, function(s) s$latent[idx])            # n x nsim
  MU  <- E * exp(ETA)
  YREP <- matrix(NA_integer_, nrow = nrow(MU), ncol = ncol(MU))
  for (j in 1:ncol(MU)) YREP[, j] <- rpois(nrow(MU), lambda = MU[, j])
  list(Yrep = YREP, Mu = MU)
}

rand_pit_from_draws <- function(y, Yrep) {
  set.seed(SEED)
  n <- length(y)
  pit <- numeric(n)
  for (i in 1:n) {
    Fi_low  <- mean(Yrep[i, ] <= (y[i] - 1))
    Fi_high <- mean(Yrep[i, ] <= y[i])
    pit[i]  <- runif(1, Fi_low, Fi_high)
  }
  pit
}

coverage_from_draws <- function(y, Yrep, probs = c(0.025, 0.25, 0.75, 0.975)) {
  qs <- t(apply(Yrep, 1, quantile, probs = probs, na.rm = TRUE))
  tibble(
    cover95 = mean(y >= qs[, 1] & y <= qs[, 4]),
    cover50 = mean(y >= qs[, 2] & y <= qs[, 3])
  )
}

err_metrics <- function(y, E, Yrep) {
  yhat <- rowMeans(Yrep)
  rate <- y / E
  rate_hat <- yhat / E
  tibble(
    MAE_y   = mean(abs(y - yhat)),
    RMSE_y  = sqrt(mean((y - yhat)^2)),
    MAE_rt  = mean(abs(rate - rate_hat)),
    RMSE_rt = sqrt(mean((rate - rate_hat)^2))
  )
}

calibrate_model_poisson <- function(fit, E, y, listw = NULL) {
  draws <- posterior_predict_inla(fit, E, nsim = NSIM)
  pit_u <- rand_pit_from_draws(y, draws$Yrep)
  ks    <- suppressWarnings(ks.test(pit_u, "punif"))
  covs  <- coverage_from_draws(y, draws$Yrep)
  errs  <- err_metrics(y, E, draws$Yrep)
  
  moran_p <- NA_real_
  if (!is.null(listw)) {
    z <- qnorm(pmin(pmax(pit_u, 1e-12), 1 - 1e-12))
    mtest <- spdep::moran.test(z, listw, alternative = "greater")
    moran_p <- mtest$p.value
  }
  
  tibble(
    DIC   = fit$dic$dic,
    WAIC  = fit$waic$waic,
    LCPO  = mean(-log(fit$cpo$cpo), na.rm = TRUE),
    ELPD  = sum(log(fit$cpo$cpo), na.rm = TRUE),
    PIT_KS_p = as.numeric(ks$p.value),
    cover50 = covs$cover50, cover95 = covs$cover95,
    MAE_y = errs$MAE_y, RMSE_y = errs$RMSE_y,
    MAE_rt = errs$MAE_rt, RMSE_rt = errs$RMSE_rt,
    Moran_p = moran_p
  )
}

# --- Grid de priors PC ---
prior_grid <- tibble::tribble(
  ~name,        ~u_prec, ~alpha_prec, ~u_phi, ~alpha_phi,
  "P1_base",       1.0,      0.01,      0.5,     2/3,
  "P2_strong",     0.5,      0.01,      0.5,     2/3,
  "P3_weak",       2.0,      0.01,      0.5,     2/3,
  "P4_lessInf",    1.0,      0.10,      0.5,     2/3,
  "P5_spatial",    1.0,      0.01,      0.7,     2/3,
  "P6_iid",        1.0,      0.01,      0.3,     2/3
)

# --- Funciones para ajustar BYM2 e iCAR con una prior del grid ---

fit_bym2 <- function(p) {
  INLA::inla(
    y_model ~ 1 + f(
      id, model = "bym2", graph = g, scale.model = TRUE,
      hyper = list(
        prec = list(prior = "pc.prec", param = c(p$u_prec, p$alpha_prec)),
        phi  = list(prior = "pc",      param = c(p$u_phi,  p$alpha_phi))
      )
    ),
    family = "poisson",
    data   = agg_depto_table,
    E      = agg_depto_table$E_pop,
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
  )
}

# Aquí el cambio: usar "besag" en lugar de "icar"
fit_icar <- function(p) {
  INLA::inla(
    y_model ~ 1 + f(
      id, model = "besag", graph = g, scale.model = TRUE,
      hyper = list(
        prec = list(prior = "pc.prec", param = c(p$u_prec, p$alpha_prec))
      )
    ),
    family = "poisson",
    data   = agg_depto_table,
    E      = agg_depto_table$E_pop,
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
  )
}

# --- Bucle principal: 6 priors × 2 modelos (BYM2, iCAR) ---
message("\n>>> Iniciando búsqueda de priors para BYM2 e iCAR...")
results_all <- vector("list", length = 0L)

for (k in seq_len(nrow(prior_grid))) {
  pri <- prior_grid[k, ]
  
  # --- BYM2 ---
  cat("\n>>> Ajustando BYM2 -", pri$name,
      sprintf("(pc.prec u=%.3f, a=%.3f; phi u=%.2f, a=%.2f)\n",
              pri$u_prec, pri$alpha_prec, pri$u_phi, pri$alpha_phi))
  fit_b <- fit_bym2(pri)
  met_b <- calibrate_model_poisson(fit_b, agg_depto_table$E_pop, agg_depto_table$y_obs, listw)
  results_all[[length(results_all) + 1L]] <-
    dplyr::bind_cols(pri, met_b) %>%
    dplyr::mutate(model = "BYM2", fit_obj = list(fit_b))
  
  # --- iCAR (Besag ICAR)
  cat("\n>>> Ajustando iCAR -", pri$name,
      sprintf("(pc.prec u=%.3f, a=%.3f)\n",
              pri$u_prec, pri$alpha_prec))
  fit_i <- fit_icar(pri)
  met_i <- calibrate_model_poisson(fit_i, agg_depto_table$E_pop, agg_depto_table$y_obs, listw)
  results_all[[length(results_all) + 1L]] <-
    dplyr::bind_cols(pri, met_i) %>%
    dplyr::mutate(model = "iCAR", fit_obj = list(fit_i))
}

res_tbl <- dplyr::bind_rows(results_all)

# --- Ranking por modelo (rank 1 = mejor dentro de cada modelo) ---
res_ranked <- res_tbl %>%
  dplyr::arrange(model, LCPO, WAIC, DIC) %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::ungroup()

message("\n>>> Resultados del grid de priors (BYM2 e iCAR):")
print(
  res_ranked %>%
    dplyr::select(model, rank, name, DIC, WAIC, LCPO, ELPD,
                  PIT_KS_p, cover50, cover95,
                  MAE_y, RMSE_y, Moran_p),
  n = Inf
)

# --- Mejores modelos por tipo (BYM2 e iCAR) ---
best_bym2_row <- res_ranked %>%
  dplyr::filter(model == "BYM2") %>%
  dplyr::slice(1)

best_icar_row <- res_ranked %>%
  dplyr::filter(model == "iCAR") %>%
  dplyr::slice(1)

best_fit <- best_bym2_row$fit_obj[[1]]
best_fit_icar <- best_icar_row$fit_obj[[1]]

message("\n>>> Mejor prior seleccionado para BYM2: ", best_bym2_row$name)
message(">>> Mejor prior seleccionado para iCAR: ", best_icar_row$name)
message("✓ Búsqueda de priors completada")
