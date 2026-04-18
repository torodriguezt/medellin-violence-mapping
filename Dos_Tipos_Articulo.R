library(dplyr)
library(stringi)
library(stringr)
library(sf)
library(spdep)
library(INLA)
library(tibble)
library(ggplot2)

source("03_preparar_datos_modelo.R")

expected <- function (population, cases, n.strata, ...) {
  
  n <- length(population) / n.strata
  E <- rep(0, n)
  qNum <- rep(0, n.strata)
  qDenom <- rep(0, n.strata)
  q <- rep(0, n.strata)
  
  # Compute strata-specific rates
  for (i in 1:n.strata) {
    indices <- rep(i, n) + seq(0, (n - 1)) * n.strata
    qNum[i] <- qNum[i] + sum(cases[indices])
    qDenom[i] <- qDenom[i] + sum(population[indices])
  }
  q <- qNum / qDenom
  
  # Compute expected counts
  for (i in 1:n) {
    indices <- (1:n.strata) + (i - 1) * n.strata
    E[i] <- sum(population[indices] * q)
  }
  
  E
}



canonize <- function(x) {
  x <- x %>%
    as.character() %>%
    stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    gsub("[[:punct:]]", " ", .) %>%
    gsub("\\s+", " ", ., perl = TRUE) %>%
    trimws()

  dplyr::case_when(
    str_detect(x, "san andres") ~ "san andres",
    str_detect(x, "bogota")     ~ "bogota dc",
    TRUE                        ~ x
  )
}

data <- readRDS("datos/Data/Espacial/SIVIGILA_debugged_spatial.rds")
poblacion <- readRDS("datos/Data/Proyecciones/proyecciones_dep_ano_edad.rds")

data <- data %>%
  mutate(
    depto_norm = canonize(departamento_ocurrencia),
    grupo_bin = if_else(
      def_naturaleza %in% c(1, 2, 3),  # no sexuales
      0L,
      1L                               # sexuales (5,6,7,10,12,14,15,...)
    )
  )

poblacion <- poblacion %>%
  mutate(depto_norm = canonize(dpnom))

poblacion_sumada <- poblacion %>%
  group_by(depto_norm) %>%
  summarise(pop_fem = sum(total_mujeres, na.rm = TRUE), .groups = "drop")

violencia_por_depto <- data %>%
  group_by(depto_norm, grupo_bin) %>%
  summarise(n = n(), .groups = "drop")

final <- violencia_por_depto %>%
  left_join(poblacion_sumada, by = "depto_norm") %>%
  mutate(tasa_por_100k = (n / pop_fem) * 100000)

final <- final %>%
  arrange(grupo_bin)

E_depto_one <- expected(
  population = final$pop_fem[1:33],
  cases      = final$n[1:33],
  n.strata   = 1
)

E_depto_six <- expected(
  population = final$pop_fem[34:66],
  cases      = final$n[34:66],
  n.strata   = 1
)

E_depto <- c(E_depto_one, E_depto_six)


depto_agg_sf <- depto_agg_sf %>%
  mutate(depto_norm = canonize(depto)) %>%
  arrange(depto_norm) %>%
  mutate(id_area = row_number())

# 2) Tabla de referencia para el join
dept_ref_sf <- depto_agg_sf %>%
  st_drop_geometry() %>%
  select(id_area, depto_norm)


dat_inla <- final %>%
  left_join(dept_ref_sf, by = "depto_norm") %>%
  filter(!is.na(id_area)) %>%
  mutate(
    id_area = as.integer(id_area),
    y       = as.integer(n),
    pop_fem = as.numeric(pop_fem),
    E       = E_depto,
    resp    = if_else(grupo_bin == 0L, 1L, 2L),
    resp_f  = factor(resp,
                     levels = c(1, 2),
                     labels = c("nat_no_sexual", "nat_sexual"))
  ) %>%
  arrange(id_area) %>%
  filter(!is.na(E), E > 0)

g <- INLA::inla.read.graph("datos/Geografia/departamentos.adj")

formula_biv <- y ~ 0 + resp_f +
  f(
    id_area,
    model       = "bym2",
    graph       = g,
    group       = resp,
    scale.model = TRUE,
    control.group = list(model = "exchangeable")
  )

model_biv <- inla(
  formula = formula_biv,
  family  = "poisson",
  data    = dat_inla,
  E       = dat_inla$E,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    config = TRUE,
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    return.marginals.predictor = TRUE 
  )
)

model_biv$summary.fitted.values[, "mean"]``

model_biv$summary.hyperpar

dat_inla <- dat_inla %>% 
  mutate(
    RR_mean = exp(model_biv$summary.linear.predictor[, "mean"]),
    RR_low  = exp(model_biv$summary.linear.predictor[, "0.025quant"]),
    RR_high = exp(model_biv$summary.linear.predictor[, "0.975quant"])
  )

library(dplyr)

top5_RR_non_sexual <- dat_inla %>%
  filter(grupo_bin == 0) %>%
  arrange(desc(RR)) %>%
  slice_head(n = 5)

top5_RR_sexual <- dat_inla %>%
  filter(grupo_bin == 1) %>%
  arrange(desc(RR)) %>%
  slice_head(n = 5)


write.csv(dat_inla, "dat_inla_resultados.csv", row.names = FALSE)


# Ahora SÍ: trabaja en escala de RR
excprob_rr <- function(res, index, thr_rr = 1) {
  sapply(
    res$marginals.fitted.values[index],
    function(marg) 1 - inla.pmarginal(q = thr_rr, marginal = marg)
  )
}

idx_y1 <- which(dat_inla$resp == 1L)
idx_y2 <- which(dat_inla$resp == 2L)


prob_rr_gt1_2 <- excprob_rr(model_biv, index = seq_len(nrow(dat_inla)), thr_rr = 1.25)

dat_inla$prob_rr_gt1_2 <- prob_rr_gt1_2

top5_prob <- dat_inla %>%
  group_by(grupo_bin) %>%
  slice_max(order_by = prob_rr_gt1_2, n = 5) %>%
  arrange(grupo_bin, desc(prob_rr_gt1_2)) %>%
  ungroup()

top5_prob <- top5_prob %>%
  select(depto_norm, grupo_bin, prob_rr_gt1_2)


top5_rr

write.csv(dat_inla, "dat_inla_resultados_con_probabilidades.csv", row.names = FALSE)

nsamp <- 1000     

samples <- inla.posterior.sample(nsamp, model_biv)

idx_pred <- grep("^Predictor", rownames(samples[[1]]$latent))

n_pred <- length(idx_pred)
stopifnot(n_pred == nrow(dat_inla)) 

eta_mat <- matrix(NA_real_, nrow = n_pred, ncol = nsamp)

for (k in seq_len(nsamp)) {
  eta_mat[, k] <- samples[[k]]$latent[idx_pred]
}

# Riesgos relativos: RR = exp(eta)
RR_mat <- exp(eta_mat)


# índices de las dos respuestas
idx_resp1 <- which(dat_inla$resp == 1L)
idx_resp2 <- which(dat_inla$resp == 2L)

eta1_mat <- eta_mat[idx_resp1, ]   # 33 x nsamp
eta2_mat <- eta_mat[idx_resp2, ]   # 33 x nsamp

# correlación espacial entre mapas en cada muestra
cor_pat <- apply(
  cbind(1:nsamp), 1,
  function(k) cor(eta1_mat[, k], eta2_mat[, k])
)

summary(cor_pat)


n_obs   <- nrow(dat_inla)   # debería ser 66
nsamp   <- ncol(eta_mat)    # ya lo definiste en 1000

# media del Poisson: mu = E * exp(eta)
mu_mat <- exp(eta_mat) * matrix(dat_inla$E, nrow = n_obs, ncol = nsamp)

set.seed(123)  # para reproducibilidad
y_rep_mat <- matrix(
  rpois(n = n_obs * nsamp, lambda = as.vector(mu_mat)),
  nrow = n_obs, ncol = nsamp
)


idx_resp1 <- which(dat_inla$resp == 1L)
idx_resp2 <- which(dat_inla$resp == 2L)

y_obs1 <- dat_inla$y[idx_resp1]
y_obs2 <- dat_inla$y[idx_resp2]

y_rep1 <- y_rep_mat[idx_resp1, ]   # 33 x nsamp
y_rep2 <- y_rep_mat[idx_resp2, ]   # 33 x nsamp


# Totales observados
tot_obs1 <- sum(y_obs1)
tot_obs2 <- sum(y_obs2)

# Totales replicados (por muestra)
tot_rep1 <- colSums(y_rep1)
tot_rep2 <- colSums(y_rep2)


pp_tot_df <- tibble(
  tot_rep1 = tot_rep1,
  tot_rep2 = tot_rep2
)

# Respuesta 1: violencia no sexual
ggplot(pp_tot_df, aes(x = tot_rep1)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  geom_vline(xintercept = tot_obs1, linetype = "dashed", linewidth = 1) +
  labs(
    x = "Total simulado (respuesta 1: no sexual)",
    y = "Frecuencia",
    title = "PPC total para respuesta 1"
  )

# Respuesta 2: violencia sexual
ggplot(pp_tot_df, aes(x = tot_rep2)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  geom_vline(xintercept = tot_obs2, linetype = "dashed", linewidth = 1) +
  labs(
    x = "Total simulado (respuesta 2: sexual)",
    y = "Frecuencia",
    title = "PPC total para respuesta 2"
  )


library(dplyr)
library(tibble)
library(ggplot2)

# how many posterior draws to use for the plot
nsamp_plot <- 50
set.seed(123)
draw_ids <- sample(seq_len(nsamp), nsamp_plot)

# simulated data (replicates) in long format - response 1
ppc_rep_list1 <- lapply(draw_ids, function(k) {
  tibble(
    y      = y_rep1[, k],
    sample = factor(k),
    tipo   = "rep"   # this label is internal, not shown in the plot
  )
})

ppc_rep1_long <- bind_rows(ppc_rep_list1)

# observed data in long format - response 1
obs1_long <- tibble(
  y    = y_obs1,
  tipo = "obs"
)

# density plot - response 1 (non-sexual)
ggplot() +
  geom_density(
    data = ppc_rep1_long,
    aes(x = y, group = sample),
    color = "grey70",
    alpha = 0.6
  ) +
  geom_density(
    data = obs1_long,
    aes(x = y),
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    x = "Counts per department (response 1: non-sexual)",
    y = "Approximate density",
    title = "Posterior predictive check (densities) - Response 1"
  )

# -------------------------------------------------------------------

# simulated data (replicates) in long format - response 2
ppc_rep_list2 <- lapply(draw_ids, function(k) {
  tibble(
    y      = y_rep2[, k],
    sample = factor(k),
    tipo   = "rep"
  )
})

ppc_rep2_long <- bind_rows(ppc_rep_list2)

# observed data in long format - response 2
obs2_long <- tibble(
  y    = y_obs2,
  tipo = "obs"
)

# density plot - response 2 (sexual)
ggplot() +
  geom_density(
    data = ppc_rep2_long,
    aes(x = y, group = sample),
    color = "grey70",
    alpha = 0.6
  ) +
  geom_density(
    data = obs2_long,
    aes(x = y),
    color = "blue",
    linewidth = 1.2
  ) +
  labs(
    x = "Counts per department (response 2: sexual)",
    y = "Approximate density",
    title = "Posterior predictive check (densities) - Response 2"
  )


model_biv$summary.hyperpar


library(ggplot2)
library(dplyr)
library(tibble)

# ------------------------------------------------------
# 1. Posterior predictive check – Response 1 (non-sexual)
# ------------------------------------------------------

obs1_long <- tibble(
  y    = y_obs1,
  tipo = "Observed data"
)

ppc_rep1_long2 <- ppc_rep1_long %>%
  mutate(tipo = "Replicated data")

p1 <- ggplot() +
  geom_density(
    data = ppc_rep1_long2,
    aes(x = y, group = sample, color = tipo),
    alpha = 0.6,
    show.legend = TRUE
  ) +
  geom_density(
    data = obs1_long,
    aes(x = y, color = tipo),
    linewidth = 1.2,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = c(
      "Replicated data" = "grey70",
      "Observed data"   = "red"
    ),
    name = "Line color"
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.2))
  ) +
  labs(
    x = "Counts per department",
    y = "Approximate density",
    title = "Posterior predictive check (densities) - Non Sexual Violence"
  ) +
  theme_minimal()

ggsave("ppc_density_response1.png", p1,
       width = 7, height = 5, dpi = 300)


# ------------------------------------------------------
# 2. Posterior predictive check – Response 2 (sexual)
# ------------------------------------------------------

ppc_rep2_long2 <- ppc_rep2_long %>%
  mutate(tipo = "Replicated data")

obs2_long <- tibble(
  y    = y_obs2,
  tipo = "Observed data"
)

p2 <- ggplot() +
  geom_density(
    data = ppc_rep2_long2,
    aes(x = y, group = sample, color = tipo),
    alpha = 0.6,
    show.legend = TRUE
  ) +
  geom_density(
    data = obs2_long,
    aes(x = y, color = tipo),
    linewidth = 1.2,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = c(
      "Replicated data" = "grey70",
      "Observed data"   = "blue"
    ),
    name = "Line color"
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.2))
  ) +
  labs(
    x = "Counts per department",
    y = "Approximate density",
    title = "Posterior predictive check (densities) - Sexual violence"
  ) +
  theme_minimal()

ggsave("ppc_density_response2.png", p2,
       width = 7, height = 5, dpi = 300)












## ============================================================
## Libraries and basic checks
## ============================================================

library(INLA)
library(dplyr)
library(tibble)

## Optional: set number of threads
## inla.setOption(num.threads = "2:1")

## Assumptions:
## - dat_inla has columns: y, E, id_area, resp, resp_f
## - g is an INLA graph object corresponding to id_area adjacency

str(dat_inla)

## Make sure resp / resp_f are factors (if not already)
dat_inla <- dat_inla %>%
  mutate(
    resp   = as.factor(resp),
    resp_f = as.factor(resp_f)
  )

## ============================================================
## 1. Baseline BYM2 model (INLA default priors)
## ============================================================

formula_biv_bym2_default <- y ~ 0 + resp_f +
  f(
    id_area,
    model       = "bym2",
    graph       = g,
    group       = resp,
    scale.model = TRUE,
    control.group = list(model = "exchangeable")
  )

model_biv_bym2_default <- inla(
  formula = formula_biv_bym2_default,
  family  = "poisson",
  data    = dat_inla,
  E       = dat_inla$E,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    config                      = TRUE,
    dic                         = TRUE,
    waic                        = TRUE,
    cpo                         = TRUE,
    return.marginals.predictor  = TRUE
  )
)

## ============================================================
## 2. BYM2 with PC priors (on precision and phi)
## ============================================================

hyper_bym2_pc <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)   # P(sigma > 1) = 0.01
  ),
  phi = list(
    prior = "pc",
    param = c(0.5, 2/3)  # P(phi < 0.5) = 2/3
  )
)

formula_biv_bym2_pc <- y ~ 0 + resp_f +
  f(
    id_area,
    model       = "bym2",
    graph       = g,
    group       = resp,
    scale.model = TRUE,
    hyper       = hyper_bym2_pc,
    control.group = list(model = "exchangeable")
  )

model_biv_bym2_pc <- inla(
  formula = formula_biv_bym2_pc,
  family  = "poisson",
  data    = dat_inla,
  E       = dat_inla$E,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    config                      = TRUE,
    dic                         = TRUE,
    waic                        = TRUE,
    cpo                         = TRUE,
    return.marginals.predictor  = TRUE
  )
)

## ============================================================
## 3. ICAR / Besag model (pure spatial, PC prior on precision)
## ============================================================

hyper_icar_pc <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)

formula_biv_icar <- y ~ 0 + resp_f +
  f(
    id_area,
    model       = "besag",   # ICAR
    graph       = g,
    group       = resp,
    scale.model = TRUE,
    hyper       = hyper_icar_pc,
    control.group = list(model = "exchangeable")
  )

model_biv_icar <- inla(
  formula = formula_biv_icar,
  family  = "poisson",
  data    = dat_inla,
  E       = dat_inla$E,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    config                      = TRUE,
    dic                         = TRUE,
    waic                        = TRUE,
    cpo                         = TRUE,
    return.marginals.predictor  = TRUE
  )
)



## ============================================================
## 5. Compare DIC and WAIC
## ============================================================

compare_criteria <- tibble(
  model = c("BYM2_default", "BYM2_PC", "ICAR_Besag"),
  DIC   = c(
    model_biv_bym2_default$dic$dic,
    model_biv_bym2_pc$dic$dic,
    model_biv_icar$dic$dic,
  ),
  WAIC  = c(
    model_biv_bym2_default$waic$waic,
    model_biv_bym2_pc$waic$waic,
    model_biv_icar$waic$waic,
  )
)

compare_criteria
