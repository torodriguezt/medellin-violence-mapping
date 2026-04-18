# --- Paquetes ---
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(spdep)
library(INLA)
library(ggplot2)
library(colorspace)
library(scales)
library(readr)

# --- Helper ---
fmt2 <- function(x) sprintf("%02d", as.integer(x))   # códigos DANE a 2 dígitos

# --- Entradas (ajusta rutas a tu entorno) ---
SIVIGILA_debugged_v2 <- readRDS("C:/Users/Tomas/Downloads/SIVIGILA_debugged_v2.rds")
carto0 <- st_read("C:/Users/Tomas/Downloads/SHP_MGN2018_INTGRD_DEPTO/MGN_ANM_DPTOS.shp", quiet = TRUE)

# Evita nombre 'data' y valida geometría
carto <- carto0 %>% st_make_valid()

# Detectar columnas de código/nombre DANE
cand_code <- c("DPTO_CCDGO","COD_DEPTO","COD_DANE","DPTO_CCDGO","COD_DPTO","DPTO")
cand_name <- c("DPTO_CNMBR","NOMBRE_DPT","NOMBRE","NOMBRE_DEP","DPTO_CNMBR")
code_col <- cand_code[cand_code %in% names(carto)][1]
name_col <- cand_name[cand_name %in% names(carto)][1]
stopifnot(!is.na(code_col))
if (is.na(name_col)) warning("No se encontró columna de nombre en el shapefile.")

carto <- carto %>%
  mutate(
    cod_dpto = fmt2(.data[[code_col]]),
    depto_sf = if (!is.na(name_col)) as.character(.data[[name_col]]) else cod_dpto
  ) %>%
  arrange(cod_dpto)

# --- Población femenina (2025) provista por ti ---
pop_fem_depto <- tribble(
  ~cod_dpto, ~depto,                ~pop_fem,
  "05","Antioquia",3540000, "08","Atlántico",1456848, "11","Bogotá D.C.",4260000,
  "13","Bolívar",1160000,   "15","Boyacá",672000,      "17","Caldas",535000,
  "18","Caquetá",213113,    "19","Cauca",806000,       "20","Cesar",714000,
  "23","Córdoba",980000,    "25","Cundinamarca",1820000,"27","Chocó",297000,
  "41","Huila",610000,      "44","La Guajira",541000,  "47","Magdalena",775000,
  "50","Meta",587000,       "52","Nariño",876000,      "54","Norte de Santander",875000,
  "63","Quindío",298000,    "66","Risaralda",499000,   "68","Santander",1216000,
  "70","Sucre",546000,      "73","Tolima",707000,      "76","Valle del Cauca",2380000,
  "81","Arauca",162000,     "85","Casanare",241000,    "86","Putumayo",200000,
  "88","San Andrés",41000,  "91","Amazonas",55000,     "94","Guainía",32000,
  "95","Guaviare",74000,    "97","Vaupés",28000,       "99","Vichada",73000
) %>% mutate(cod_dpto = fmt2(cod_dpto)) %>% arrange(cod_dpto)

# =====================================================
# 1) (YA LA TIENES) Conteo por residencia:
#    conteo_residencia_named con columnas: cod_dpto_r, depto, n
#    Si quieres consistencia con el offset femenino, arma esa tabla con sexo=="F".
# =====================================================
# AQUI asumimos que ya existe `conteo_residencia_named`

conteo_residencia_named <- SIVIGILA_debugged_v2 %>%
  filter(!is.na(cod_dpto_r), cod_dpto_r != "0") %>%  
  group_by(cod_dpto_r) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(pop_fem_depto %>% select(cod_dpto, depto),
            by = c("cod_dpto_r" = "cod_dpto")) %>%
  select(cod_dpto_r, depto, n) %>%
  arrange(desc(n))

# Normalizamos nombres/estructura para unir
y_depto <- conteo_residencia_named %>%
  transmute(
    cod_dpto = fmt2(cod_dpto_r),
    y_obs    = as.integer(n)
  )

# =====================================================
# 2) Uniones y consistencia con cartografía/población
# =====================================================
df_base <- pop_fem_depto %>%
  select(cod_dpto, depto, pop_fem) %>%
  left_join(y_depto, by = "cod_dpto") %>%
  mutate(y_obs = replace_na(y_obs, 0L)) %>%  # si algún dpto no aparece en conteo
  arrange(cod_dpto)

faltan_en_mapa <- anti_join(df_base, carto %>% st_drop_geometry() %>% select(cod_dpto), by = "cod_dpto")
sobran_en_mapa <- anti_join(carto %>% st_drop_geometry() %>% select(cod_dpto), df_base, by = "cod_dpto")
if (nrow(faltan_en_mapa) > 0) stop("Códigos en población sin geometría: ", paste(faltan_en_mapa$cod_dpto, collapse=", "))
if (nrow(sobran_en_mapa) > 0) message("Aviso: códigos en shapefile sin población: ", paste(sobran_en_mapa$cod_dpto, collapse=", "))

# Mantener intersección y adjuntar geometría
depto_agg_sf <- carto %>%
  inner_join(df_base, by = "cod_dpto") %>%
  arrange(cod_dpto) %>%
  mutate(
    id        = row_number(),
    # y_model >= 1 (por si hubiera algún cero residual)
    y_model   = y_obs,
    E_pop     = pop_fem,
    E_classic = pop_fem * (sum(y_obs)/sum(pop_fem))  # opcional
  ) %>%
  relocate(id, cod_dpto, depto, y_obs, y_model, pop_fem, E_pop, E_classic)

# =====================================================
# 3) Grafo de vecindad
# =====================================================
nb0 <- poly2nb(as_Spatial(depto_agg_sf), queen = TRUE, snap = 1e-08)
W  <- nb2mat(nb0, style = "B", zero.policy = TRUE)  # inspección opcional
tmp_graph <- tempfile(fileext = ".graph")
nb2INLA(file = tmp_graph, nb = nb0)
g <- INLA::inla.read.graph(tmp_graph)

# Tabla para INLA
agg_depto_table <- depto_agg_sf %>%
  st_drop_geometry() %>%
  select(id, cod_dpto, depto, y_obs, y_model, pop_fem, E_pop, E_classic)

# =====================================================
# 4) Ajustes: Poisson-BYM2 vs ZIP-BYM2 (usando y_model)
# =====================================================
fit_pois <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE),
  family = "poisson",
  data   = agg_depto_table,
  E      = agg_depto_table$E_pop,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE)
)

fit_zip <- INLA::inla(
  y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE),
  family = "zeroinflatedpoisson1",
  data   = agg_depto_table,
  E      = agg_depto_table$E_pop,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE)
)

metrics_from_fit <- function(f, name){
  data.frame(
    model = name,
    DIC   = f$dic$dic,
    pDIC  = f$dic$p.eff,
    WAIC  = f$waic$waic,
    pWAIC = f$waic$p.eff,
    LCPO  = mean(-log(f$cpo$cpo), na.rm=TRUE),
    ELPD  = sum(log(f$cpo$cpo),  na.rm=TRUE)
  )
}
cmp <- bind_rows(
  metrics_from_fit(fit_pois, "Poisson-BYM2"),
  metrics_from_fit(fit_zip,  "ZIP-BYM2")
)
print(cmp)

# =====================================================
# 5) RR y ranking (ambos modelos)
# =====================================================
make_rr_tbl <- function(fit, label, base_tbl = agg_depto_table, sf = depto_agg_sf){
  lin   <- fit$summary.linear.predictor
  r_nac <- sum(base_tbl$y_model)/sum(base_tbl$E_pop)   # anclado al promedio nacional del set
  log_r <- log(r_nac)
  
  RR_mean <- exp(lin$mean) / r_nac
  RR_lcl  <- exp(lin$`0.025quant`) / r_nac
  RR_ucl  <- exp(lin$`0.975quant`) / r_nac
  
  Pr_RR_gt1 <- if (!is.null(fit$marginals.linear.predictor) &&
                   length(fit$marginals.linear.predictor) == nrow(base_tbl)) {
    vapply(fit$marginals.linear.predictor,
           function(m) 1 - INLA::inla.pmarginal(log_r, m), numeric(1))
  } else {
    1 - pnorm(log_r, mean = lin$mean, sd = lin$sd)
  }
  
  sf %>%
    st_drop_geometry() %>%
    transmute(
      model = label,
      cod_dpto, depto,
      y_obs, y_model, pop_fem,
      RR = RR_mean, RR_lcl = RR_lcl, RR_ucl = RR_ucl,
      Pr_RR_gt1 = Pr_RR_gt1
    ) %>%
    arrange(desc(RR))
}

rr_pois <- make_rr_tbl(fit_pois, "Poisson-BYM2")
rr_zip  <- make_rr_tbl(fit_zip,  "ZIP-BYM2")

# Ranking (ZIP por defecto)
rr_zip_rank <- rr_zip %>%
  mutate(
    Rank = row_number(),
    RR   = round(RR, 3),
    RR_lcl = round(RR_lcl, 3),
    RR_ucl = round(RR_ucl, 3),
    Pr_RR_gt1 = percent(pmin(pmax(Pr_RR_gt1,0),1), accuracy = 0.1)
  ) %>%
  select(Rank, depto, cod_dpto, y_obs, y_model, pop_fem, RR, RR_lcl, RR_ucl, Pr_RR_gt1)

rr_zip_rank
# =====================================================
# 6) Mapas de RR (Poisson y ZIP)
# =====================================================
plot_rr_map <- function(fit, titulo, sf = depto_agg_sf, base_tbl = agg_depto_table){
  lin   <- fit$summary.linear.predictor
  r_nac <- sum(base_tbl$y_model)/sum(base_tbl$E_pop)
  RR    <- exp(lin$mean)/r_nac
  Pr    <- if (!is.null(fit$marginals.linear.predictor)) {
    vapply(fit$marginals.linear.predictor,
           function(m) 1 - INLA::inla.pmarginal(log(r_nac), m), numeric(1))
  } else 1 - pnorm(log(r_nac), mean=lin$mean, sd=lin$sd)
  m <- sf %>% mutate(RR = RR, Pr_RR_gt1 = pmin(pmax(Pr,0),1))
  rmin <- min(m$RR, na.rm=TRUE); rmax <- max(m$RR, na.rm=TRUE)
  
  ggplot(m) +
    geom_sf(aes(fill = RR), color = "white", size = 0.2) +
    scale_fill_gradientn(
      colours = divergingx_hcl(11, palette = "Geyser"),
      values  = scales::rescale(c(rmin, 1, rmax)),
      name    = "RR"
    ) +
    labs(title = titulo, subtitle = "RR = exp(η) / tasa nacional (residencia)") +
    theme_minimal()
}

p_rr_pois <- plot_rr_map(fit_pois, "Riesgo relativo — Poisson-BYM2 (Residencia)")
p_rr_zip  <- plot_rr_map(fit_zip,  "Riesgo relativo — ZIP-BYM2 (Residencia)")
print(p_rr_pois); print(p_rr_zip)

# (Opcional) mapa de probabilidad de exceso para ZIP
lin_zip <- fit_zip$summary.linear.predictor
r_nac_zip <- sum(agg_depto_table$y_model)/sum(agg_depto_table$E_pop)
Pr_zip <- if (!is.null(fit_zip$marginals.linear.predictor)) {
  vapply(fit_zip$marginals.linear.predictor,
         function(m) 1 - INLA::inla.pmarginal(log(r_nac_zip), m), numeric(1))
} else 1 - pnorm(log(r_nac_zip), mean = lin_zip$mean, sd = lin_zip$sd)
map_zip <- depto_agg_sf %>% mutate(Pr_RR_gt1 = pmin(pmax(Pr_zip,0),1))

ggplot(map_zip) +
  geom_sf(aes(fill = Pr_RR_gt1), color = "white", size = 0.2) +
  scale_fill_viridis_c(name = "P(RR>1)", limits = c(0,1), labels = scales::percent) +
  labs(title = "ZIP-BYM2: Probabilidad de exceso de riesgo (Residencia)") +
  theme_minimal()








# =====================================================
# 4) GRID DE PRIORS (Poisson-BYM2) Y BÚSQUEDA DEL MEJOR
# =====================================================

INLA::inla.setOption(num.threads = "2:2")  # ajusta a tu máquina

# --- Utilidades para predicción y calibración (solo Poisson) ---
ilogit <- function(x) 1/(1+exp(-x))

posterior_predict_inla <- function(fit, E, nsim = 3000L){
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

rand_pit_from_draws <- function(y, Yrep){
  set.seed(123)
  n <- length(y); pit <- numeric(n)
  for (i in 1:n){
    Fi_low  <- mean(Yrep[i,] <= (y[i]-1))
    Fi_high <- mean(Yrep[i,] <=  y[i])
    pit[i]  <- runif(1, Fi_low, Fi_high)
  }
  pit
}

coverage_from_draws <- function(y, Yrep, probs = c(0.025, 0.25, 0.75, 0.975)){
  qs <- t(apply(Yrep, 1, quantile, probs = probs, na.rm = TRUE))
  tibble(
    cover95 = mean(y >= qs[,1] & y <= qs[,4]),
    cover50 = mean(y >= qs[,2] & y <= qs[,3])
  )
}

err_metrics <- function(y, E, Yrep){
  yhat <- rowMeans(Yrep)
  rate <- y / E; rate_hat <- yhat / E
  tibble(
    MAE_y   = mean(abs(y - yhat)),
    RMSE_y  = sqrt(mean((y - yhat)^2)),
    MAE_rt  = mean(abs(rate - rate_hat)),
    RMSE_rt = sqrt(mean((rate - rate_hat)^2))
  )
}

brier_excess <- function(y, E, Yrep){
  rate_nat <- sum(y)/sum(E)
  evt_obs  <- as.integer(y > E * rate_nat)
  p_evt    <- rowMeans(Yrep > (E * rate_nat))
  tibble(Brier_exceso = mean((p_evt - evt_obs)^2))
}

calibrate_model_poisson <- function(fit, E, y, listw = NULL){
  draws <- posterior_predict_inla(fit, E, nsim = 3000L)
  pit_u <- rand_pit_from_draws(y, draws$Yrep)
  ks    <- suppressWarnings(ks.test(pit_u, "punif"))
  covs  <- coverage_from_draws(y, draws$Yrep)
  errs  <- err_metrics(y, E, draws$Yrep)
  bri   <- brier_excess(y, E, draws$Yrep)
  
  moran_p <- NA_real_
  if (!is.null(listw)) {
    z <- qnorm(pmin(pmax(pit_u, 1e-12), 1-1e-12))
    mtest <- spdep::moran.test(z, listw, alternative = "greater")
    moran_p <- mtest$p.value
  }
  
  tibble(
    DIC   = fit$dic$dic,
    WAIC  = fit$waic$waic,
    LCPO  = mean(-log(fit$cpo$cpo), na.rm=TRUE),
    ELPD  = sum(log(fit$cpo$cpo),  na.rm=TRUE),
    PIT_KS_p = as.numeric(ks$p.value),
    cover50 = covs$cover50, cover95 = covs$cover95,
    MAE_y = errs$MAE_y, RMSE_y = errs$RMSE_y,
    MAE_rt = errs$MAE_rt, RMSE_rt = errs$RMSE_rt,
    Brier_exceso = bri$Brier_exceso,
    Moran_p = moran_p
  )
}

# --- Lista de priors a explorar (PC-PRIORS) ---
# prec (tau): prior="pc.prec", param=c(u, alpha) => P(1/sqrt(tau) > u) = alpha
# phi (mezcla BYM2 en [0,1]): prior="pc", param=c(u, alpha)  (p.ej. P(phi<0.5)=2/3)
prior_grid <- tibble::tribble(
  ~name,        ~u_prec, ~alpha_prec, ~u_phi, ~alpha_phi,
  "P1_base",       1.0,      0.01,      0.5,     2/3,
  "P2_strong",     0.5,      0.01,      0.5,     2/3,
  "P3_weak",       2.0,      0.01,      0.5,     2/3,
  "P4_lessInf",    1.0,      0.10,      0.5,     2/3,
  "P5_spatial",    1.0,      0.01,      0.7,     2/3,  # a priori más peso estructurado
  "P6_iid",        1.0,      0.01,      0.3,     2/3   # a priori más peso no estructurado
)

# --- Vecindario para Moran en residuales (opcional) ---
listw <- spdep::nb2listw(nb0, style="W", zero.policy=TRUE)

# --- Función para ajustar con una prior del grid ---
fit_one_prior <- function(p){
  INLA::inla(
    y_model ~ 1 + f(id, model = "bym2", graph = g, scale.model = TRUE,
                    hyper = list(
                      prec = list(prior = "pc.prec", param = c(p$u_prec, p$alpha_prec)),
                      phi  = list(prior = "pc",      param = c(p$u_phi,  p$alpha_phi))
                    )),
    family = "poisson",
    data   = agg_depto_table,
    E      = agg_depto_table$E_pop,
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE)
  )
}

# --- Bucle principal: ajusta, calibra y guarda métricas ---
results <- vector("list", nrow(prior_grid))

for (k in seq_len(nrow(prior_grid))){
  pri <- prior_grid[k,]
  cat("\n>>> Ajustando", pri$name, 
      sprintf("(pc.prec u=%.3f, a=%.3f; phi u=%.2f, a=%.2f)\n",
              pri$u_prec, pri$alpha_prec, pri$u_phi, pri$alpha_phi))
  
  fit <- fit_one_prior(pri)
  met <- calibrate_model_poisson(fit, agg_depto_table$E_pop, agg_depto_table$y_obs, listw)
  results[[k]] <- dplyr::bind_cols(pri, met) %>% dplyr::mutate(fit_obj = list(fit))
}

res_tbl <- dplyr::bind_rows(results)

# --- Ranking: menor LCPO (mejor predicción) -> menor WAIC -> menor DIC
res_ranked <- res_tbl %>%
  dplyr::arrange(LCPO, WAIC, DIC) %>%
  dplyr::mutate(rank = dplyr::row_number())

print(res_ranked %>% dplyr::select(rank, name, DIC, WAIC, LCPO, ELPD, 
                                   PIT_KS_p, cover50, cover95, 
                                   MAE_y, RMSE_y, Brier_exceso, Moran_p))

# --- Elige el ganador ---
best_row <- res_ranked %>% dplyr::slice(1)
best_fit <- best_row$fit_obj[[1]]
best_name <- best_row$name
cat("\n>>> Mejor prior:", best_name, "\n")

# =====================================================
# 5) MAPA RR con el mejor ajuste
# =====================================================
plot_rr_map <- function(fit, titulo, sf = depto_agg_sf, base_tbl = agg_depto_table){
  lin   <- fit$summary.linear.predictor
  r_nac <- sum(base_tbl$y_model)/sum(base_tbl$E_pop)
  RR    <- exp(lin$mean)/r_nac
  m <- sf %>% dplyr::mutate(RR = RR)
  rmin <- min(m$RR, na.rm=TRUE); rmax <- max(m$RR, na.rm=TRUE)
  
  ggplot(m) +
    geom_sf(aes(fill = RR), color = "white", size = 0.2) +
    scale_fill_gradientn(
      colours = colorspace::divergingx_hcl(11, palette = "Geyser"),
      values  = scales::rescale(c(rmin, 1, rmax)),
      name    = "RR"
    ) +
    labs(title = titulo, subtitle = "RR = exp(η) / tasa nacional (residencia)") +
    theme_minimal()
}

p_best <- plot_rr_map(best_fit, paste0("Riesgo relativo — Poisson-BYM2 (", best_name, ")"))
print(p_best)


# Hiperparámetros BYM2
hp <- fit_pois$summary.hyperpar
hp[, c("mean","0.025quant","0.975quant")]

depto_agg_sf$eta_mean <- fit_pois$summary.random$id$mean
ggplot(depto_agg_sf) +
  geom_sf(aes(fill = eta_mean), color="white", size=0.2) +
  scale_fill_gradient2(name="η (random)", midpoint=0) +
  labs(title="Efecto espacial (BYM2) — Poisson") + theme_minimal()

cpo_tbl <- tibble(
  depto   = depto_agg_sf$depto,
  cod     = depto_agg_sf$cod_dpto,
  lcpo    = -log(fit_pois$cpo$cpo)
) |> arrange(desc(lcpo))
head(cpo_tbl, 10)  # Top 10 más “sorpresivos” para el modelo


exceed_map <- function(fit, thr = 1.2, sf = depto_agg_sf, base_tbl = agg_depto_table){
  lin   <- fit$summary.linear.predictor
  r_nat <- sum(base_tbl$y_model)/sum(base_tbl$E_pop)
  cut   <- log(thr * r_nat)
  Pr    <- if (!is.null(fit$marginals.linear.predictor)) {
    vapply(fit$marginals.linear.predictor,
           function(m) 1 - INLA::inla.pmarginal(cut, m), numeric(1))
  } else 1 - pnorm(cut, mean=lin$mean, sd=lin$sd)
  m <- sf |> mutate(Pr_exc = pmin(pmax(Pr,0),1))
  ggplot(m) + geom_sf(aes(fill=Pr_exc), color="white", size=0.2) +
    scale_fill_viridis_c(labels=scales::percent, limits=c(0,1), name=paste0("P(RR>",thr,")")) +
    labs(title=paste0("Excedencia de riesgo (umbral ",thr,")")) + theme_minimal()
}
print(exceed_map(best_fit, thr=1.2))


draws <- posterior_predict_inla(fit_pois, agg_depto_table$E_pop, nsim = 2000L)
obs_zeros   <- sum(agg_depto_table$y_obs == 0)
pred_zeros  <- mean(colSums(draws$Yrep == 0))
c(obs_zeros = obs_zeros, pred_zeros_mean = pred_zeros)
# Si el ZIP reduce fuertemente el gap, es evidencia a su favor.

nb_rook <- spdep::poly2nb(as_Spatial(depto_agg_sf), queen = FALSE)
tmp_g2  <- tempfile(fileext=".graph"); spdep::nb2INLA(tmp_g2, nb_rook)
g2 <- INLA::inla.read.graph(tmp_g2)

fit_pois_rook <- INLA::inla(
  y_model ~ 1 + f(id, model="bym2", graph=g2, scale.model=TRUE),
  family="poisson", data=agg_depto_table, E=agg_depto_table$E_pop,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)
)

bind_rows(
  metrics_from_fit(fit_pois, "Poisson-BYM2 (Queen)"),
  metrics_from_fit(fit_pois_rook, "Poisson-BYM2 (Rook)")
)

fit_pois_Eclassic <- INLA::inla(
  y_model ~ 1 + f(id, model="bym2", graph=g, scale.model=TRUE),
  family="poisson", data=agg_depto_table, E=agg_depto_table$E_classic,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)
)
bind_rows(
  metrics_from_fit(fit_pois, "Poisson (E_pop)"),
  metrics_from_fit(fit_pois_Eclassic, "Poisson (E_classic)")
)


fit_nb <- INLA::inla(
  y_model ~ 1 + f(id, model="bym2", graph=g, scale.model=TRUE),
  family="nbinomial", data=agg_depto_table, E=agg_depto_table$E_pop,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)
)

fit_zinb <- INLA::inla(
  y_model ~ 1 + f(id, model="bym2", graph=g, scale.model=TRUE),
  family="zeroinflatednbinomial1", data=agg_depto_table, E=agg_depto_table$E_pop,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE)
)

bind_rows(
  metrics_from_fit(fit_pois, "Poisson"),
  metrics_from_fit(fit_zip,  "ZIP"),
  metrics_from_fit(fit_nb,   "NegBin"),
  metrics_from_fit(fit_zinb, "ZINB")
)

S   <- INLA::inla.posterior.sample(2000, result = best_fit, seed = 1)
idx <- grep("^Predictor", rownames(S[[1]]$latent))
ETA <- sapply(S, function(s) s$latent[idx])             # n_area x nsim
r_nac <- sum(agg_depto_table$y_model)/sum(agg_depto_table$E_pop)
RRd <- exp(ETA) / r_nac                                  # n_area x nsim
ranks <- apply(RRd, 2, function(x) rank(-x, ties.method="average")) # por draw
P_top3 <- rowMeans(ranks <= 3)
topk_tbl <- depto_agg_sf |> st_drop_geometry() |>
  transmute(depto, cod_dpto, P_top3 = scales::percent(P_top3, accuracy=0.1)) |>
  arrange(desc(P_top3))
head(topk_tbl, 10)


r_nat <- sum(agg_depto_table$y_model)/sum(agg_depto_table$E_pop)
raw_RR <- (agg_depto_table$y_model/agg_depto_table$E_pop) / r_nat
smth_RR <- exp(best_fit$summary.linear.predictor$mean) / r_nat

plot_df <- tibble(raw_RR=raw_RR, smth_RR=smth_RR, depto=depto_agg_sf$depto)
ggplot(plot_df, aes(raw_RR, smth_RR, label=depto)) +
  geom_abline(slope=1, intercept=0, linetype=2) +
  geom_point() + 
  labs(x="RR crudo", y="RR suavizado", title="Shrinkage de tasas") + theme_minimal()


sc <- function(x) exp(-0.5*(x - min(x)))        # Softmax estable para WAIC
W <- c(Pois=sc(fit_pois$waic$waic), ZIP=sc(fit_zip$waic$waic))
W <- W / sum(W)

linP <- fit_pois$summary.linear.predictor$mean
linZ <- fit_zip$summary.linear.predictor$mean
r_nat <- sum(agg_depto_table$y_model)/sum(agg_depto_table$E_pop)
RR_ens <- (W["Pois"]*exp(linP) + W["ZIP"]*exp(linZ)) / r_nat
depto_agg_sf$RR_ensemble <- RR_ens
