library(dplyr)
library(stringi)
library(stringr)
library(INLA)
library(sf)
library(spdep)
library(tibble)

# --------------------
# Normalización
# --------------------
norm_name <- function(x) {
  x %>%
    as.character() %>%
    stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    trimws() %>%
    gsub("\\s+", " ", ., perl = TRUE)
}

canonize <- function(x) {
  x <- x %>%
    norm_name() %>%
    gsub("[[:punct:]]", " ", .) %>%
    gsub("\\s+", " ", ., perl = TRUE) %>%
    trimws()

  dplyr::case_when(
    str_detect(x, "san andres") ~ "san andres",
    str_detect(x, "bogota")     ~ "bogota dc",
    TRUE                        ~ x
  )
}

# --------------------
# Población 2023
# --------------------
poblacion <- poblacion %>%
  mutate(depto_norm = canonize(dpnom)) %>%
  group_by(depto_norm) %>%
  summarise(pop_fem = sum(total_mujeres, na.rm = TRUE), .groups = "drop")

# --------------------
# Casos 2023 (nat 1 y 6) agregados por depto
# --------------------
casos <- data %>%
  mutate(depto_norm = canonize(departamento_ocurrencia)) %>%
  group_by(depto_norm) %>%
  summarise(y = n(), .groups = "drop")

# --------------------
# Unir casos + población
# --------------------
depto_dat <- casos %>%
  full_join(poblacion, by = "depto_norm") %>%
  mutate(
    y       = if_else(is.na(y), 0L, as.integer(y)),
    pop_fem = as.numeric(pop_fem)
  ) %>%
  filter(!is.na(pop_fem), pop_fem > 0)

# --------------------
# Shapefile + id_area
# --------------------
depto_agg_sf <- depto_agg_sf %>%
  mutate(depto_norm = canonize(depto_norm)) %>%
  arrange(depto_norm) %>%
  mutate(id_area = row_number())

dept_ref_sf <- depto_agg_sf %>%
  st_drop_geometry() %>%
  select(id_area, depto_norm)

depto_inla <- depto_dat %>%
  left_join(dept_ref_sf, by = "depto_norm") %>%
  filter(!is.na(id_area)) %>%
  mutate(id_area = as.integer(id_area))

# --------------------
# Esperados a nivel de depto (usando expected CORRECTAMENTE)
# --------------------
E_depto <- expected(
  population = depto_inla$pop_fem,
  cases      = depto_inla$y,
  n.strata   = 1
)

depto_inla <- depto_inla %>%
  mutate(E = E_depto)

# Chequeo rápido
summary(depto_inla$y)
summary(depto_inla$E)

# --------------------
# Modelo BYM2 univariado
# --------------------
g <- INLA::inla.read.graph("datos/Geografia/departamentos.adj")

formula_univ <- y ~ 1 +
  f(
    id_area,
    model       = "bym2",
    graph       = g,
    scale.model = TRUE,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi  = list(prior = "pc",      param = c(0.5, 2/3))
    )
  )

model_univ <- inla(
  formula = formula_univ,
  family  = "poisson",
  data    = depto_inla,
  E       = E_depto,   # OJO: aquí va E_depto, no "E" viejo
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

mu_hat_depto <- model_univ$summary.fitted.values[, "mean"]