# =====================================================
# 09_predicciones_2030.R
# Predicciones con Poisson-BYM2 para población proyectada 2030 (Mujeres DANE)
# =====================================================

library(tibble)
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(ggplot2)
library(sf)
library(viridis)

message("\n>>> Cargando datos y modelo para predicción 2030...")

# -----------------------------------------------------
# 0. REQUISITOS
# -----------------------------------------------------
# Este script asume que ya existen en el ambiente:
# - fit_pois          : modelo Poisson-BYM2 ajustado sobre 33 departamentos
# - agg_depto_table   : tabla base con 33 filas, contiene al menos:
#                         id, y_obs, E_pop y una columna 'depto' (nombre dpto)
# - depto_agg_sf      : objeto sf con geometría de los 33 departamentos
#                       y columna 'id' alineada con agg_depto_table
# - INLA cargado
# - El orden de filas de agg_depto_table es el orden de las áreas en fit_pois
#
# Rutas de entrada:
# - Proyecciones DANE:
#   "datos/PPED-AreaSexoEdadDep-2018-2050_VP.xlsx"
#
# Si necesitas cargar objetos, por ejemplo:
# fit_pois        <- readRDS("ruta/fit_pois.rds")
# agg_depto_table <- readRDS("ruta/agg_depto_table.rds")
# depto_agg_sf    <- readRDS("ruta/depto_agg_sf.rds")


# =====================================================
# 1. DEFINIR POBLACIÓN PROYECTADA 2030 (MUJERES) DESDE DANE
# =====================================================

message("\n>>> Leyendo proyecciones DANE 2018–2050...")

# 1) Leer la hoja completa sin encabezados
raw <- read_excel(
  "datos/PPED-AreaSexoEdadDep-2018-2050_VP.xlsx",
  sheet = "PobDepartamentalxÁreaSexoEdad",
  col_names = FALSE
)

# 2) Detectar la fila donde comienzan los datos reales (primer DP = 05)
start_row <- raw %>% 
  mutate(row = row_number()) %>% 
  filter(str_trim(`...1`) == "05") %>% 
  slice(1) %>% 
  pull(row)

# 3) Encabezado real está justo una fila arriba
header_row <- start_row - 1
raw_header  <- as.character(raw[header_row, ])

# 4) Limpiar encabezado (reemplazar NA y "" por nombres válidos tipo V1, V2, ...)
clean_header <- raw_header
clean_header[is.na(clean_header) | clean_header == ""] <- 
  paste0("V", which(is.na(clean_header) | clean_header == ""))

# 5) Construir la tabla final con encabezados correctos
data_clean <- raw %>%
  slice(start_row:n()) %>% 
  setNames(clean_header) %>% 
  filter(!is.na(!!sym(clean_header[1])))   # filtrar filas vacías en la primera columna

# 6) Filtrar solo año 2030 y Área Geográfica "Total"
data_2030_total <- data_clean %>%
  filter(V3 == 2030, V4 == "Total")

# 7) Quitar columnas que contengan "Hombres" o "Total"
#    (Nos quedamos con Mujeres y sus desgloses, si existieran)
data_2030_filtrado <- data_2030_total %>%
  select(
    -contains("Hombres"),
    -contains("Total")
  )

# 8) Renombrar columnas principales
data_2030_final <- data_2030_filtrado %>%
  rename(
    DP           = V1,   # código DANE, por si quieres revisar luego
    Departamento = V2,
    Ano          = V3,
    AreaGeo      = V4
  )

# 9) Limpiar nombres de departamento para el join
data_2030_final <- data_2030_final %>%
  mutate(
    Departamento_clean = str_squish(str_to_lower(Departamento)),
    
    # Arreglar manualmente nombres problemáticos para que coincidan con 'depto'
    Departamento_clean = case_when(
      Departamento_clean == "bogotá, d.c." ~ "bogotá d.c.",
      Departamento_clean == "archipiélago de san andrés, providencia y santa catalina" ~ "san andrés",
      TRUE ~ Departamento_clean
    )
  )

# 10) Asegurar que la columna Mujeres sea numérica
if ("Mujeres" %in% names(data_2030_final)) {
  data_2030_final <- data_2030_final %>%
    mutate(Mujeres = readr::parse_number(as.character(Mujeres)))
} else {
  stop("No se encontró la columna 'Mujeres' en data_2030_final. Revisa los nombres de columnas.")
}

message("\n>>> Resumen de proyecciones DANE 2030 (Mujeres, Total, por departamento):")
print(
  data_2030_final %>% 
    select(Departamento, Ano, AreaGeo, Mujeres) %>% 
    head()
)

# 11) Preparar vector E_pop_2030 a partir de Mujeres y alinearlo con agg_depto_table
#     Join por nombre: 'depto' en agg_depto_table y 'Departamento' en data_2030_final

agg_depto_table <- agg_depto_table %>%
  mutate(
    depto_clean = str_squish(str_to_lower(depto))
  )

proj_2030 <- data_2030_final %>%
  select(Departamento_clean, E_pop_2030 = Mujeres)

agg_depto_table_2030 <- agg_depto_table %>%
  left_join(proj_2030, by = c("depto_clean" = "Departamento_clean"))

# Chequeo rápido
na_join <- agg_depto_table_2030 %>%
  filter(is.na(E_pop_2030)) %>%
  select(depto, depto_clean)

if (nrow(na_join) > 0) {
  warning("Hay departamentos en agg_depto_table sin match en proyecciones DANE (E_pop_2030 = NA). Revisa los nombres de 'depto' vs 'Departamento'.\n")
  print(na_join)
}

E_pop_2030 <- agg_depto_table_2030$E_pop_2030

message("\n>>> Población femenina proyectada 2030 (E_pop_2030) unida a tabla base:")
print(
  agg_depto_table_2030 %>% 
    select(id, depto, E_pop, E_pop_2030) %>% 
    head()
)


# =====================================================
# 2. FUNCIÓN ROBUSTA: posterior_predict_inla()
# =====================================================

posterior_predict_inla <- function(fit, E, nsim = 3000) {
  
  if (is.null(fit$misc$configs)) {
    stop("El modelo debe tener control.compute=list(config=TRUE)")
  }
  
  S <- INLA::inla.posterior.sample(n = nsim, result = fit)
  print(S)
  
  contents <- fit$misc$configs$contents
  idx_pred <- which(contents$tag == "Predictor")
  
  start <- contents$start[idx_pred]
  len   <- contents$length[idx_pred]
  end   <- start + len - 1
  
  idx <- start:end   # en tu caso, 1:33 si tienes 33 áreas
  
  ETA <- sapply(S, function(s) s$latent[idx])
  MU  <- E * exp(ETA)
  Yrep <- apply(MU, 2, function(lambda) rpois(length(lambda), lambda))
  
  list(Yrep = Yrep, Mu = MU)
}


# =====================================================
# 3. FUNCIÓN: TABLA DE INTERVALOS DE PREDICCIÓN
# =====================================================

make_prediction_tbl <- function(fit,
                                E,
                                base_tbl,
                                nsim = 3000,
                                probs = c(0.025, 0.5, 0.975)) {
  
  draws <- posterior_predict_inla(fit, E, nsim)
  
  Yrep <- draws$Yrep
  MU   <- draws$Mu
  
  if (nrow(Yrep) != nrow(base_tbl)) {
    stop(paste0("ERROR: el modelo tiene ", nrow(Yrep),
                " áreas, pero la tabla base tiene ",
                nrow(base_tbl), "."))
  }
  
  qs_y  <- t(apply(Yrep, 1, quantile, probs = probs))
  qs_mu <- t(apply(MU,   1, quantile, probs = probs))
  
  tibble(
    id          = base_tbl$id,
    y_obs       = base_tbl$y_obs,
    E_pop_2030  = E,
    # Intervalos de PREDICCIÓN del conteo Y^{2030}
    y_low       = qs_y[,1],
    y_med       = qs_y[,2],
    y_high      = qs_y[,3],
    # Intervalos de CREDIBILIDAD para la media µ^{2030}
    mu_low      = qs_mu[,1],
    mu_med      = qs_mu[,2],
    mu_high     = qs_mu[,3]
  )
}


# =====================================================
# 4. GENERAR PREDICCIONES PARA 2030
# =====================================================

message("\n>>> Generando intervalos de predicción para 2030 usando proyecciones DANE (Mujeres)...")

pred_2030 <- make_prediction_tbl(
  fit      = fit_pois,
  E        = E_pop_2030,
  base_tbl = agg_depto_table_2030,
  nsim     = 3000
)

message("\n>>> Predicción 2030 completada ✓")
print(pred_2030)


# =====================================================
# 5. MAPA: TASA CRUDA PREDICHA 2030 (CASOS POR 100.000 MUJERES)
# =====================================================

message("\n>>> Creando mapa de tasa cruda predicha 2030...")

# 5.1. Calcular tasa cruda predicha por departamento
pred_2030_map <- pred_2030 %>%
  mutate(
    tasa_cruda_pred_2030 = (y_med / E_pop_2030) * 1e5   # casos por 100.000 mujeres
  )

# 5.2. Unir con el sf de departamentos
# Se asume que 'depto_agg_sf' tiene una columna 'id' compatible con agg_depto_table
depto_agg_sf_2030 <- depto_agg_sf %>%
  left_join(pred_2030_map, by = "id")

# 5.3. Graficar el mapa (similar a tu ejemplo de tasa cruda 2021–2023)
p_mapa_pred_2030 <- ggplot(depto_agg_sf_2030) +
  geom_sf(aes(fill = tasa_cruda_pred_2030),
          color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option   = "cividis",
    name     = "Crude rate per 100,000",
    na.value = "grey85"
  ) +
  labs(
    title    = "Crude predicted rate of gender-based violence, 2030"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.text   = element_text(size = 8),
    legend.title  = element_text(size = 9, face = "bold"),
    axis.text     = element_text(size = 7),
    axis.title    = element_text(size = 8),
    legend.position = "right",
    panel.grid      = element_blank()
  )

print(p_mapa_pred_2030)


depto_agg_sf_2030_filtrado <- depto_agg_sf_2030[, c("depto", "y_low", "y_high")]

# (Opcional) guardar figura
# ggsave("Figures/FigureX_TasaCrudaPred_2030.png",
#        p_mapa_pred_2030, width = 8, height = 6, dpi = 300)
