library(dplyr)
library(stringi)
library(stringr)
library(sf)
library(spdep)
library(INLA)
library(tibble)

# ----------------------------
# 1. Función única de nombres
# ----------------------------
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

# ----------------------------
# 2. Cargar datos
# ----------------------------
data <- readRDS("datos/Data/Espacial/SIVIGILA_debugged_spatial.rds")
poblacion <- readRDS("datos/Data/Proyecciones/proyecciones_dep_ano_edad.rds")

# ----------------------------
# 3. Normalizar nombres
# ----------------------------
data <- data %>%
  mutate(depto_norm = canonize(departamento_ocurrencia))

poblacion <- poblacion %>%
  mutate(depto_norm = canonize(dpnom))

# ----------------------------
# 4. Sumar población femenina
# ----------------------------
poblacion_sumada <- poblacion %>%
  group_by(depto_norm) %>%
  summarise(pop_fem = sum(total_mujeres, na.rm = TRUE), .groups = "drop")

# ----------------------------
# 5. Casos por depto y tipo
# ----------------------------
violencia_por_depto <- data %>%
  group_by(depto_norm, def_naturaleza) %>%
  summarise(n = n(), .groups = "drop")

# ----------------------------
# 6. Unir casos + población
# ----------------------------
final <- violencia_por_depto %>%
  left_join(poblacion_sumada, by = "depto_norm") %>%
  mutate(tasa_por_100k = (n / pop_fem) * 100000)

# Top 2 tipos por depto
final_top2 <- final %>%
  filter(def_naturaleza %in% c(1, 6)) %>%
  group_by(depto_norm) %>%
  arrange(def_naturaleza) %>%  # garantiza orden 1,6
  ungroup()


# ----------------------------
# 7. dat_inla
# ----------------------------
dat_inla <- final_top2 %>%
  group_by(depto_norm) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(resp = row_number()) %>%
  ungroup() %>%
  mutate(
    y      = as.integer(n),
    E      = as.numeric(pop_fem),
    resp_f = factor(resp)
  ) %>%
  filter(!is.na(y), !is.na(E), E > 0)

# ----------------------------
# 8. Shapefile agregado
# ----------------------------
depto_agg_sf <- depto_agg_sf %>%
  mutate(
    depto_norm = canonize(depto)
  ) %>%
  arrange(depto_norm) %>%
  mutate(id_area = row_number())

# ----------------------------
# 9. Chequeo de nombres
# ----------------------------
anti_join(
  dat_inla %>% distinct(depto_norm),
  depto_agg_sf %>% st_drop_geometry() %>% distinct(depto_norm),
  by = "depto_norm"
)

# Grafo de vecindad (Queen)
nb0 <- poly2nb(as_Spatial(depto_agg_sf), queen = TRUE, snap = 1e-08)
nb2INLA(file = "datos/Geografia/departamentos.adj", nb = nb0)
g <- INLA::inla.read.graph("datos/Geografia/departamentos.adj")

# Referencia id_area–depto_norm desde el sf
dept_ref_sf <- depto_agg_sf %>%
  st_drop_geometry() %>%
  select(id_area, depto_norm)

dat_inla <- final_top2 %>%
  mutate(
    depto_norm = canonize(depto_norm),
    depto_norm = str_squish(depto_norm),
    resp = if_else(def_naturaleza == 1, 1L, 2L),
    resp_f = factor(
      resp,
      levels = c(1, 2),
      labels = c("nat_1", "nat_6")
    )
  ) %>%
  left_join(dept_ref_sf, by = "depto_norm") %>%
  mutate(
    y = as.integer(n),
    E = as.numeric(pop_fem)
  ) %>%
  filter(!is.na(id_area), E > 0)

# Chequeo de consistencia
max(dat_inla$id_area); g$n   # max(id_area) debe ser <= g$n
sapply(dat_inla[, c("y", "E", "id_area", "resp")], anyNA)

# ============================
# 3. Ajustar INLA bivariado
# ============================
formula_biv <- y ~ 0 + resp_f +
  f(
    id_area,
    model = "bym2",
    graph = g,
    group = resp,
    scale.model = TRUE,
    control.group = list(model = "exchangeable"),
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi  = list(prior = "pc",      param = c(0.5, 2/3))
    )
  )


res_biv <- inla(
  formula_biv,
  family = "poisson",
  data   = dat_inla,
  E      = E,  # exposición = pop_fem sumada 2013–2023
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

fitted_df <- as_tibble(res_biv$summary.fitted.values)
lin <- res_biv$summary.linear.predictor

r_nat  <- sum(dat_inla$y) / sum(dat_inla$E)
log_r  <- log(r_nat)
RR_mean <- exp(lin$mean) / r_nat

RR_mean

RR_mean <- res_biv$summary.fitted.values$mean