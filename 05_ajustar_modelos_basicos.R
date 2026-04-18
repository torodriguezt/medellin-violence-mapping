# =====================================================
# 05_ajustar_modelos_basicos.R
# Ajuste de modelos Poisson-BYM2 y ZIP-BYM2 básicos
# =====================================================

source("04_vecindad_grafo.R")

# --- Función para extraer métricas ---
metrics_from_fit <- function(f, name) {
  data.frame(
    model = name,
    DIC   = f$dic$dic,
    pDIC  = f$dic$p.eff,
    WAIC  = f$waic$waic,
    pWAIC = f$waic$p.eff,
    LCPO  = mean(-log(f$cpo$cpo), na.rm = TRUE),
    ELPD  = sum(log(f$cpo$cpo), na.rm = TRUE)
  )
}

# --- Modelo Poisson-BYM2 ---
message("Ajustando modelo Poisson-BYM2...")
fit_pois <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE),
  family = "poisson",
  data   = agg_depto_table,
  E      = agg_depto_table$E_pop,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)

# --- Modelo ZIP-BYM2 ---
message("Ajustando modelo ZIP-BYM2...")
fit_zip <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE),
  family = "zeroinflatedpoisson1",
  data   = agg_depto_table,
  E      = agg_depto_table$E_pop,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)

# --- Comparación de modelos ---
message("\n>>> Comparación de modelos básicos:")
cmp <- bind_rows(
  metrics_from_fit(fit_pois, "Poisson-BYM2"),
  metrics_from_fit(fit_zip,  "ZIP-BYM2")
)
print(cmp)

# --- Hiperparámetros BYM2 ---
message("\n>>> Hiperparámetros Poisson-BYM2:")
hp <- fit_pois$summary.hyperpar
print(hp[, c("mean", "0.025quant", "0.975quant")])

message("✓ Modelos básicos ajustados")
