# =====================================================
# 07_analisis_sensibilidad.R
# Análisis de sensibilidad adicionales
# =====================================================

source("06_busqueda_priors.R")

# --- 1. Conteo de ceros ---
message("\n>>> Análisis de ceros...")
draws <- posterior_predict_inla(fit_pois, agg_depto_table$E_pop, nsim = 2000L)
obs_zeros   <- sum(agg_depto_table$y_obs == 0)
pred_zeros  <- mean(colSums(draws$Yrep == 0))
message(sprintf("  - Ceros observados: %d", obs_zeros))
message(sprintf("  - Ceros predichos (media): %.1f", pred_zeros))

# --- 2. Comparación Rook vs Queen ---
message("\n>>> Comparación Rook vs Queen...")
fit_pois_rook <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g2, scale.model = TRUE),
  family = "poisson", 
  data = agg_depto_table, 
  E = agg_depto_table$E_pop,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

cmp_vecindad <- bind_rows(
  metrics_from_fit(fit_pois, "Poisson-BYM2 (Queen)"),
  metrics_from_fit(fit_pois_rook, "Poisson-BYM2 (Rook)")
)
print(cmp_vecindad)

# --- 3. Comparación de offsets ---
message("\n>>> Comparación E_pop vs E_classic...")
fit_pois_Eclassic <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE),
  family = "poisson", 
  data = agg_depto_table, 
  E = agg_depto_table$E_classic,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

cmp_offset <- bind_rows(
  metrics_from_fit(fit_pois, "Poisson (E_pop)"),
  metrics_from_fit(fit_pois_Eclassic, "Poisson (E_classic)")
)
print(cmp_offset)

# --- 4. Modelos alternativos: NegBin y ZINB ---
message("\n>>> Ajustando modelos alternativos...")

fit_nb <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE),
  family = "nbinomial", 
  data = agg_depto_table, 
  E = agg_depto_table$E_pop,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

fit_zinb <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE),
  family = "zeroinflatednbinomial1", 
  data = agg_depto_table, 
  E = agg_depto_table$E_pop,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

cmp_familias <- bind_rows(
  metrics_from_fit(fit_pois, "Poisson"),
  metrics_from_fit(fit_zip,  "ZIP"),
  metrics_from_fit(fit_nb,   "NegBin"),
  metrics_from_fit(fit_zinb, "ZINB")
)
print(cmp_familias)

# --- 5. CPO por departamento ---
message("\n>>> Departamentos con peor ajuste (CPO)...")
cpo_tbl <- tibble(
  depto   = depto_agg_sf$depto,
  cod     = depto_agg_sf$cod_dpto,
  lcpo    = -log(fit_pois$cpo$cpo)
) %>% arrange(desc(lcpo))

print(head(cpo_tbl, 10))

# --- 6. Ensemble de modelos ---
message("\n>>> Calculando ensemble de modelos...")
sc <- function(x) exp(-0.5 * (x - min(x)))
W <- c(Pois = sc(fit_pois$waic$waic), ZIP = sc(fit_zip$waic$waic))
W <- W / sum(W)

linP <- fit_pois$summary.linear.predictor$mean
linZ <- fit_zip$summary.linear.predictor$mean
r_nat <- sum(agg_depto_table$y_model) / sum(agg_depto_table$E_pop)
RR_ens <- (W["Pois"] * exp(linP) + W["ZIP"] * exp(linZ)) / r_nat
depto_agg_sf$RR_ensemble <- RR_ens

message(sprintf("  - Peso Poisson: %.3f", W["Pois"]))
message(sprintf("  - Peso ZIP: %.3f", W["ZIP"]))

message("✓ Análisis de sensibilidad completado")
