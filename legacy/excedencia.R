# =====================================================
# 08_exceedance.R
# Exceedance probabilities for relative risk (RR)
# =====================================================

# This script:
#  - Computes exceedance probabilities P(RR_i > thr) for each department
#  - Produces a table of RR and exceedance probabilities
#  - Generates an exceedance probability map
#
# Requirements (already created elsewhere in your workflow):
#   - fit_pois        : INLA model object (Poisson-BYM2)
#   - agg_depto_table : data.frame with columns y_model, E_pop, etc.
#   - depto_agg_sf    : sf object with geometry and cod_dpto, depto
#
# If needed, source the script where these are defined, e.g.:
# source("07_analisis_sensibilidad.R")

library(dplyr)
library(ggplot2)
library(sf)
library(scales)

# =====================================================
# 1. Exceedance table: P(RR_i > thr)
# =====================================================

# Computes exceedance probabilities P(RR_i > thr) for each department,
# where RR_i = r_i / r_nat and r_nat is the national (overall) rate.
#
# thr is specified on the RR scale (e.g., thr = 1.1 means 10% above
# the national rate).
#
# We use the marginal posterior distribution of the linear predictor:
#   eta_i = log(r_i),
# and compute:
#   P(RR_i > thr) = P(log r_i > log(thr * r_nat))
# via inla.pmarginal(), following the approach in the reference article.

compute_exceed_tbl <- function(fit,
                               thr = 1.1,
                               base_tbl = agg_depto_table,
                               sf = depto_agg_sf) {
  
  # Posterior summary of the linear predictor (eta_i = log r_i)
  lin   <- fit$summary.linear.predictor
  
  # National rate: r_nat = sum(y) / sum(E)
  r_nat <- sum(base_tbl$y_model) / sum(base_tbl$E_pop)
  
  # Threshold on the log-risk scale:
  # RR_i = r_i / r_nat  ->  r_i > thr * r_nat  -> log r_i > log(thr * r_nat)
  cut   <- log(thr * r_nat)
  
  # Exceedance probabilities using inla.pmarginal():
  # P(RR_i > thr) = P(log r_i > cut)
  if (!is.null(fit$marginals.linear.predictor)) {
    excprob <- sapply(
      fit$marginals.linear.predictor,
      function(marg) {
        1 - INLA::inla.pmarginal(q = cut, marginal = marg)
      }
    )
  } else {
    # Fallback using normal approximation if marginals are not stored
    excprob <- 1 - pnorm(cut, mean = lin$mean, sd = lin$sd)
  }
  
  # Posterior mean RR_i = r_i / r_nat
  RR_mean <- exp(lin$mean) / r_nat
  
  sf %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      cod_dpto,
      depto,
      RR     = RR_mean,
      Pr_exc = pmin(pmax(excprob, 0), 1)  # numeric safety: clamp to [0,1]
    ) %>%
    dplyr::arrange(dplyr::desc(Pr_exc))
}

# =====================================================
# 2. Exceedance probability map
# =====================================================

# Produces a choropleth map of P(RR_i > thr) by department.

exceed_map <- function(fit,
                       thr = 1.1,
                       sf = depto_agg_sf,
                       base_tbl = agg_depto_table) {
  
  exc_tbl <- compute_exceed_tbl(
    fit      = fit,
    thr      = thr,
    base_tbl = base_tbl,
    sf       = sf
  )
  
  # Join exceedance probabilities to the spatial object
  m <- sf %>%
    dplyr::left_join(
      exc_tbl %>% dplyr::select(cod_dpto, Pr_exc),
      by = "cod_dpto"
    )
  
  ggplot(m) + 
    geom_sf(aes(fill = Pr_exc), color = "white", size = 0.25) +
    scale_fill_viridis_c(
      option = "cividis",
      labels = scales::percent,
      limits = c(0, 1),
      name   = paste0("P(RR > ", thr, ")")
    ) +
    labs(
      title = paste0("Exceedance Probability")
    ) +
    theme_minimal()
}

# =====================================================
# 3. Example: run exceedance analysis for Poisson-BYM2
# =====================================================

# Here we assume your fitted model is stored in `fit_pois`
# (for example, Poisson-BYM2 model on aggregated female counts).

message("\n>>> Computing exceedance table for Poisson-BYM2...")

thr_exc <- 1.5  # threshold on the RR scale

exc_tbl <- compute_exceed_tbl(
  fit      = fit_pois,
  thr      = thr_exc,
  base_tbl = agg_depto_table,
  sf       = depto_agg_sf
)

# Show top 10 departments by exceedance probability
top10_exc <- exc_tbl %>%
  dplyr::mutate(
    RR     = round(RR, 3),
    Pr_exc = scales::percent(Pr_exc, accuracy = 0.1)
  ) %>%
  dplyr::slice_head(n = 10)

message("\n>>> Top 10 departments by exceedance probability (RR > ", thr_exc, "):")
print(top10_exc)

# Plot exceedance map
message("\n>>> Generating exceedance probability map...")
p_exc <- exceed_map(
  fit      = fit_pois,
  thr      = thr_exc,
  sf       = depto_agg_sf,
  base_tbl = agg_depto_table
)
print(p_exc)

# Optionally, save the map:
ggsave("Figuras/Figura_exceedance_map.png", p_exc, width = 7, height = 5)

message("\n✓ Exceedance probabilities and map successfully generated.")
