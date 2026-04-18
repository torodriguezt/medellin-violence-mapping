# =====================================================
# 07_ppc_densidades.R
# Posterior Predictive Check basado en densidades
# =====================================================

# Este script:
#  - Carga el mejor modelo (best_fit) desde 06_busqueda_priors.R
#  - Genera 3000 réplicas de la posterior predictiva
#  - Compara la densidad de los datos observados con las densidades
#    de muchas réplicas (PPC funcional)
#  - Hace unos checks básicos de variabilidad entre réplicas

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tibble)

# -----------------------------------------------------
# 1. Cargar modelo y utilidades
# -----------------------------------------------------
# Esto te deja disponibles:
#   - best_fit           (mejor modelo BYM2)
#   - agg_depto_table    (con y_obs y E_pop)
#   - posterior_predict_inla()

# Si quisieras usar el mejor iCAR:
# fit_ppc <- best_fit_icar
fit_ppc <- best_fit

# Datos observados (conteos y exposiciones)
y_obs_counts <- agg_depto_table$y_obs
E_pop        <- agg_depto_table$E_pop

# -----------------------------------------------------
# 2. Elegir si trabajar con conteos o con tasas
# -----------------------------------------------------

USE_RATES <- TRUE   # TRUE = usar tasas, FALSE = usar conteos

if (USE_RATES) {
  y_obs <- y_obs_counts / E_pop
  response_label <- "Rate (Y/E)"
} else {
  y_obs <- y_obs_counts
  response_label <- "Conteos (Y)"
}

# -----------------------------------------------------
# 3. Simular desde la posterior predictiva (3000 réplicas)
# -----------------------------------------------------

NSIM_PPC <- 300L
set.seed(123)

draws_ppc <- posterior_predict_inla(
  fit  = fit_ppc,
  E    = E_pop,
  nsim = NSIM_PPC
)

Yrep_counts <- draws_ppc$Yrep

if (USE_RATES) {
  # Convertir réplicas a tasas Y/E
  Yrep <- sweep(Yrep_counts, 1, E_pop, "/")
} else {
  Yrep <- Yrep_counts
}

# -----------------------------------------------------
# 4. Checks rápidos de variabilidad (para tu tranquilidad)
# -----------------------------------------------------

cat("\n>>> CHECKS RÁPIDOS DE VARIABILIDAD EN LAS RÉPLICAS\n")

# ¿alguna réplica es exactamente igual a los datos observados?
identica_a_obs <- apply(Yrep, 2, function(col) all(col == y_obs))
cat("¿Alguna réplica idéntica a y_obs? :", any(identica_a_obs), "\n")

# Varianza de un estadístico global (suma)
T_sum_obs <- sum(y_obs)
T_sum_rep <- colSums(Yrep)

cat("Suma observada           :", T_sum_obs, "\n")
cat("Resumen suma simulada    :\n")
print(summary(T_sum_rep))
cat("Desviación estándar suma :", sd(T_sum_rep), "\n")

# -----------------------------------------------------
# 5. Construir densidades: observada + réplicas
# -----------------------------------------------------

# Rango común de valores para todas las densidades
all_vals <- c(y_obs, as.numeric(Yrep))
rng      <- range(all_vals)

# Densidad de los datos observados
dens_obs <- density(y_obs, from = rng[1], to = rng[2], n = 512)
x_grid   <- dens_obs$x

# Para no sobrecargar el gráfico, graficamos solo algunas réplicas
N_PLOT <- 300L
idx_plot <- sample(seq_len(ncol(Yrep)), size = min(N_PLOT, ncol(Yrep)))

# Función que calcula densidad en el mismo grid para cada réplica
dens_from_col <- function(j) {
  d <- density(Yrep[, j], from = rng[1], to = rng[2], n = 512)
  tibble(
    x    = d$x,
    dens = d$y,
    tipo = "replicado",
    sim  = j
  )
}

df_rep <- map_dfr(idx_plot, dens_from_col)

df_obs <- tibble(
  x    = x_grid,
  dens = dens_obs$y,
  tipo = "observado",
  sim  = 0L
)

df_all <- bind_rows(df_rep, df_obs)

# -----------------------------------------------------
# 6. Gráfico: densidad observada vs réplicas
# -----------------------------------------------------

gg_ppc_dens <- ggplot() +
  # Posterior predictive densities (black lines)
  geom_line(
    data = df_all %>% filter(tipo == "replicado"),
    aes(
      x = x, y = dens, group = sim,
      color = "Posterior predictive samples"
    ),
    alpha = 0.10
  ) +
  # Observed density (red line)
  geom_line(
    data = df_all %>% filter(tipo == "observado"),
    aes(
      x = x, y = dens,
      color = "Observed data"
    ),
    linewidth = 1.2
  ) +
  scale_color_manual(
    name = "",  # empty title
    values = c(
      "Observed data" = "red",
      "Posterior predictive samples" = "black"
    )
  ) +
  labs(
    x = response_label,
    y = "Density",
    title = "Posterior Predictive Check"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

print(gg_ppc_dens)

