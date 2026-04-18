library(dplyr)
library(stringi)
library(stringr)
library(INLA)
library(sf)
library(spdep)
library(tibble)

data <- readRDS("datos/Data/Espacial/SIVIGILA_debugged_spatial.rds")
poblacion <- readRDS("datos/Data/Proyecciones/proyecciones_dep_ano_edad.rds")

# -------------------------------
# Normalización y canonización
# -------------------------------
norm_name <- function(x) {
  x %>%
    as.character() %>%
    stri_trans_general("Latin-ASCII") %>%  # quita tildes
    tolower() %>%
    trimws() %>%
    gsub("\\s+", " ", ., perl = TRUE)
}

canonize <- function(x) {
  x <- x %>%
    norm_name() %>%                       # usamos la misma normalización base
    gsub("[[:punct:]]", " ", .) %>%
    gsub("\\s+", " ", ., perl = TRUE) %>%
    trimws()

  dplyr::case_when(
    str_detect(x, "san andres") ~ "san andres",
    str_detect(x, "bogota")     ~ "bogota dc",
    TRUE                        ~ x
  )
}

# -------------------------------
# Población 2023
# -------------------------------
poblacion_2023 <- poblacion %>%
  filter(ano == 2023) %>%
  mutate(dpnom_norm = canonize(dpnom)) %>%
  group_by(dpnom_norm) %>%
  summarise(pop_fem = sum(total_mujeres, na.rm = TRUE), .groups = "drop")

# -------------------------------
# Casos 2023 agregados
# -------------------------------
agg_2023 <- data %>%
  filter(ano_hecho == 2023) %>%
  mutate(depto_norm = canonize(departamento_ocurrencia)) %>%
  group_by(depto_norm, def_naturaleza) %>%
  summarise(n = n(), .groups = "drop")

# Por si acaso, ver qué no matchea con población
unmatched <- agg_2023 %>%
  distinct(depto_norm) %>%
  anti_join(poblacion_2023, by = c("depto_norm" = "dpnom_norm"))
print(unmatched)

# -------------------------------
# Bivariado + tasa
# -------------------------------
bivariado_2023 <- agg_2023 %>%
  left_join(poblacion_2023, by = c("depto_norm" = "dpnom_norm")) %>%
  mutate(tasa_por_100k = if_else(!is.na(pop_fem) & pop_fem > 0,
                                 (n / pop_fem) * 100000,
                                 NA_real_)) %>%
  mutate(departamento_ocurrencia = depto_norm) %>%
  relocate(departamento_ocurrencia, def_naturaleza, n, pop_fem, tasa_por_100k)

bivariado_top2 <- bivariado_2023 %>%
  group_by(departamento_ocurrencia) %>%
  slice_max(order_by = n, n = 2, with_ties = FALSE) %>%
  ungroup()

# -------------------------------
# Shapefile: depto_agg_sf (AQUÍ asumo que ya lo cargaste antes)
# -------------------------------

# 1) Canonizamos usando la misma función
depto_agg_sf <- depto_agg_sf %>%
  mutate(depto_norm = canonize(depto_norm)) %>%
  arrange(depto_norm) %>%
  mutate(id_area = row_number())

# 2) Tabla de referencia para el join
dept_ref_sf <- depto_agg_sf %>%
  st_drop_geometry() %>%
  select(id_area, depto_norm)

# Si quieres comprobar matcheos:
bivariado_top2 %>%
  distinct(depto_norm) %>%
  anti_join(dept_ref_sf, by = "depto_norm") %>%
  print()

# -------------------------------
# Construir dat_inla
# -------------------------------
dat_inla <- bivariado_top2 %>%
  mutate(
    depto_norm              = canonize(depto_norm),
    departamento_ocurrencia = depto_norm
  ) %>%
  left_join(dept_ref_sf, by = "depto_norm") %>%
  group_by(depto_norm) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(
    resp = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    y      = as.integer(n),
    E      = as.numeric(pop_fem),
    resp_f = factor(resp)
  ) %>%
  filter(!is.na(id_area), !is.na(E), E > 0)

# Verifica que ya no está vacío
dat_inla
sapply(dat_inla[, c("y", "E", "id_area", "resp")], anyNA)


max(dat_inla$id_area); g$n

formula_biv <- y ~ 0 + resp_f +
  f(id_area,
    model  = "bym2",
    graph  = g,
    group  = resp,
    control.group = list(model = "exchangeable")
  )

res_biv <- inla(
  formula_biv,
  family = "poisson",
  data   = dat_inla,
  E      = E,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

fitted_df <- as_tibble(res_biv$summary.fitted.values)

stopifnot(nrow(fitted_df) == nrow(dat_inla))

rr_df <- dat_inla %>%
  select(
    id_area,
    depto_norm,
    departamento_ocurrencia,
    def_naturaleza,
    resp,
    E
  ) %>%
  bind_cols(fitted_df) %>%
  transmute(
    id_area,
    depto_norm,
    departamento_ocurrencia,
    def_naturaleza,
    resp,
    # Riesgo relativo (tasa ajustada) directamente desde INLA
    RR_mean = mean,
    RR_lcl  = `0.025quant`,
    RR_ucl  = `0.975quant`
  )

rr_df %>%
  arrange(depto_norm, resp) %>%
  print(n = 20)

rr_df <- rr_df %>%
  mutate(
    tasa_ajustada_100k_mean = RR_mean * 1e5,
    tasa_ajustada_100k_lcl  = RR_lcl  * 1e5,
    tasa_ajustada_100k_ucl  = RR_ucl  * 1e5
  )

rr_df %>%
  select(
    depto_norm,
    def_naturaleza,
    resp,
    tasa_ajustada_100k_mean,
    tasa_ajustada_100k_lcl,
    tasa_ajustada_100k_ucl
  ) %>%
  arrange(depto_norm, resp) %>%
  print(n = 20)