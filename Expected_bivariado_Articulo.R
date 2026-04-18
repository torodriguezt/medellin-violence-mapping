library(dplyr)
library(stringi)
library(stringr)
library(sf)
library(spdep)
library(INLA)
library(tibble)
library(ggplot2)

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
  mutate(depto_norm = canonize(departamento_ocurrencia))

poblacion <- poblacion %>%
  mutate(depto_norm = canonize(dpnom))

poblacion_sumada <- poblacion %>%
  group_by(depto_norm) %>%
  summarise(pop_fem = sum(total_mujeres, na.rm = TRUE), .groups = "drop")

violencia_por_depto <- data %>%
  group_by(depto_norm, def_naturaleza) %>%
  summarise(n = n(), .groups = "drop")

final <- violencia_por_depto %>%
  left_join(poblacion_sumada, by = "depto_norm") %>%
  mutate(tasa_por_100k = (n / pop_fem) * 100000)

final_top2 <- final %>%
  filter(def_naturaleza %in% c(1, 6)) %>%
  group_by(depto_norm) %>%
  arrange(def_naturaleza) %>%  
  ungroup()

tasas_nat <- final_top2 %>%
  group_by(def_naturaleza) %>%
  summarise(
    r_nat = sum(n) / sum(pop_fem),
    .groups = "drop"
  )

E_depto_one <- expected(
  population = final_top2$pop_fem[1:33],
  cases      = final_top2$n[1:33],
  n.strata   = 1
)

E_depto_six <- expected(
  population = final_top2$pop_fem[34:66],
  cases      = final_top2$n[34:66],
  n.strata   = 1
)

E_depto <- c(E_depto_one, E_depto_six)

dat_inla <- final_top2 %>%
  left_join(dept_ref_sf, by = "depto_norm") %>%
  left_join(tasas_nat,    by = "def_naturaleza") %>%
  filter(!is.na(id_area)) %>%
  mutate(
    id_area = as.integer(id_area),
    y       = as.integer(n),
    pop_fem = as.numeric(pop_fem),
    E       = E_depto,
    resp    = if_else(def_naturaleza == 1L, 1L, 2L),
    resp_f  = factor(resp,
                     levels = c(1, 2),
                     labels = c("nat_1", "nat_6"))
  ) %>%
  arrange(id_area, resp) %>%
  filter(!is.na(E), E > 0)

summary(dat_inla$y)
summary(dat_inla$E)

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
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

mu_hat_depto <- model_biv$summary.fitted.values[, "mean"]

eta_hat <- model_biv$summary.linear.predictor[, "mean"]

RR_hat  <- exp(eta_hat)

summary(RR_hat)

dat_inla <- dat_inla %>%
  mutate(
    eta_hat = eta_hat,
    RR      = RR_hat
  )

write.csv(dat_inla, "dat_inla_resultados.csv", row.names = FALSE)

library(sf)
shp <- st_read("SHP_MGN2018_INTGRD_DEPTO/MGN_ANM_DPTOS.shp")

# asegúrate de tener depto_norm en el shp
shp <- shp %>% mutate(depto_norm = canonize(DPTO_CNMBR))

st_write(shp, "departamentos.geojson", driver = "GeoJSON")
