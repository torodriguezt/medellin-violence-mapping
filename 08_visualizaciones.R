# =====================================================
# 08_visualizaciones.R  (versión limpia: solo rr_pois)
# =====================================================

source("07_analisis_sensibilidad.R")

# =====================================================
# 1. TABLA DE RIESGO RELATIVO
# =====================================================

lin <- fit_pois$summary.linear.predictor
r_nat <- sum(agg_depto_table$y_model) / sum(agg_depto_table$E_pop)
log_r <- log(r_nat)

RR_mean <- exp(lin$mean) / r_nat
RR_lcl <- exp(lin$`0.025quant`) / r_nat

make_rr_tbl <- function(fit, label,
                        base_tbl = agg_depto_table,
                        sf = depto_agg_sf) {
  lin <- fit$summary.linear.predictor
  r_nat <- sum(base_tbl$y_model) / sum(base_tbl$E_pop)
  log_r <- log(r_nat)

  RR_mean <- exp(lin$mean) / r_nat
  RR_lcl <- exp(lin$`0.025quant`) / r_nat
  RR_ucl <- exp(lin$`0.975quant`) / r_nat

  # Probabilidad RR > 1
  Pr_RR_gt1 <- if (!is.null(fit$marginals.linear.predictor)) {
    vapply(
      fit$marginals.linear.predictor,
      function(m) 1 - INLA::inla.pmarginal(log_r, m), numeric(1)
    )
  } else {
    1 - pnorm(log_r, mean = lin$mean, sd = lin$sd)
  }

  sf %>%
    st_drop_geometry() %>%
    transmute(
      model = label,
      cod_dpto, depto,
      y_obs, y_model, pop_fem,
      RR = RR_mean,
      RR_lcl = RR_lcl,
      RR_ucl = RR_ucl,
      Pr_RR_gt1 = Pr_RR_gt1
    ) %>%
    arrange(desc(RR))
}

# =====================================================
# 1.1 TABLA DE EXCEDENCIA (RR > thr)
# =====================================================

compute_exceed_tbl <- function(fit,
                               thr = 1.1,
                               base_tbl = agg_depto_table,
                               sf = depto_agg_sf) {
  lin <- fit$summary.linear.predictor # eta_i = log r_i
  r_nat <- sum(base_tbl$y_model) / sum(base_tbl$E_pop)
  cut <- log(thr * r_nat)

  # P(RR_i > thr)
  Pr <- if (!is.null(fit$marginals.linear.predictor)) {
    vapply(
      fit$marginals.linear.predictor,
      function(m) 1 - INLA::inla.pmarginal(cut, m),
      numeric(1)
    )
  } else {
    1 - pnorm(cut, mean = lin$mean, sd = lin$sd)
  }

  RR_mean <- exp(lin$mean) / r_nat

  sf %>%
    st_drop_geometry() %>%
    transmute(
      cod_dpto,
      depto,
      RR     = RR_mean,
      Pr_exc = pmin(pmax(Pr, 0), 1)
    ) %>%
    arrange(desc(Pr_exc))
}

# =====================================================
# 2. MAPA DE RIESGO RELATIVO
# =====================================================

plot_rr_map <- function(fit,
                        title = "Relative Risk",
                        sf = depto_agg_sf,
                        base_tbl = agg_depto_table) {
  lin <- fit$summary.linear.predictor
  r_nat <- sum(base_tbl$y_model) / sum(base_tbl$E_pop)
  RR <- exp(lin$mean) / r_nat

  # Probabilidad RR > 1
  Pr <- if (!is.null(fit$marginals.linear.predictor)) {
    vapply(
      fit$marginals.linear.predictor,
      function(m) 1 - INLA::inla.pmarginal(log(r_nat), m), numeric(1)
    )
  } else {
    1 - pnorm(log(r_nat), mean = lin$mean, sd = lin$sd)
  }

  m <- sf %>% mutate(
    RR = RR,
    Pr_RR_gt1 = pmin(pmax(Pr, 0), 1)
  )

  ggplot(m) +
    geom_sf(aes(fill = RR), color = "white", size = .25) +
    scale_fill_viridis_c(
      option = "cividis",
      name = "Relative Risk (RR)",
      na.value = "grey85"
    ) +
    labs(title = title) +
    theme_minimal()
}

# =====================================================
# 3. MAPA DE PROBABILIDAD DE EXCEDENCIA
# =====================================================

exceed_map <- function(fit,
                       thr = 1.1,
                       sf = depto_agg_sf,
                       base_tbl = agg_depto_table) {
  exc_tbl <- compute_exceed_tbl(fit,
    thr = thr,
    base_tbl = base_tbl,
    sf = sf
  )

  # Unimos la prob. de excedencia a la geometría
  m <- sf %>%
    left_join(
      exc_tbl %>% select(cod_dpto, Pr_exc),
      by = "cod_dpto"
    )

  ggplot(m) +
    geom_sf(aes(fill = Pr_exc), color = "white", size = .25) +
    scale_fill_viridis_c(
      option = "cividis",
      labels = scales::percent,
      limits = c(0, 1),
      name = paste0("P(RR > ", thr, ")")
    ) +
    labs(title = paste0("Exceedance Probability (Threshold = ", thr, ")")) +
    theme_minimal()
}

# =====================================================
# 4. GENERAR TODO (solo Poisson BYM2)
# =====================================================

message("\n>>> Generando tabla de RR para Poisson-BYM2...")
rr_pois <- make_rr_tbl(fit_pois, "Poisson-BYM2")

rr_rank <- rr_pois %>%
  mutate(
    Rank = row_number(),
    RR = round(RR, 3),
    RR_lcl = round(RR_lcl, 3),
    RR_ucl = round(RR_ucl, 3),
    Pr_RR_gt1 = percent(Pr_RR_gt1, accuracy = 0.1)
  ) %>%
  select(
    Rank, depto, cod_dpto, y_obs, y_model, pop_fem,
    RR, RR_lcl, RR_ucl, Pr_RR_gt1
  )

print(rr_rank)

message("\n>>> Generando mapa de RR...")
p_rr <- plot_rr_map(fit_pois, "Riesgo relativo — Poisson-BYM2")
print(p_rr)

message("\n>>> Generando mapa de excedencia...")
thr_exc <- 1.1
p_exc <- exceed_map(fit_pois, thr = thr_exc)
print(p_exc)

# --- Top 10 por probabilidad de excedencia (RR > thr_exc) ---
message("\n>>> Top 10 departamentos por probabilidad de excedencia (RR > ", thr_exc, ")...")
exc_tbl <- compute_exceed_tbl(fit_pois, thr = thr_exc)

top10_exc <- exc_tbl %>%
  mutate(
    RR     = round(RR, 3),
    Pr_exc = scales::percent(Pr_exc, accuracy = 0.1)
  ) %>%
  dplyr::slice_head(n = 10)

print(top10_exc)

message("✓ Visualizaciones y ranking de excedencia generados")
