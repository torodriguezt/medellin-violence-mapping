# =====================================================
# 02_procesar_geometria.R
# Procesamiento del shapefile de departamentos
# =====================================================

source("01_cargar_datos.R")

# --- Cargar shapefile ---
message("Cargando shapefile de departamentos...")
carto0 <- st_read(RUTAS$shapefile, quiet = TRUE)
carto <- carto0 %>% st_make_valid()

# --- Detectar columnas de código/nombre DANE ---
cand_code <- COLUMNAS_SHAPEFILE$candidatos_codigo
cand_name <- COLUMNAS_SHAPEFILE$candidatos_nombre

code_col <- cand_code[cand_code %in% names(carto)][1]
name_col <- cand_name[cand_name %in% names(carto)][1]

stopifnot(!is.na(code_col))
if (is.na(name_col)) warning("No se encontró columna de nombre en el shapefile.")

# --- Normalizar códigos y nombres ---
message("Normalizando códigos departamentales...")
carto <- carto %>%
  mutate(
    cod_dpto = fmt2(.data[[code_col]]),
    depto_sf = if (!is.na(name_col)) as.character(.data[[name_col]]) else cod_dpto
  ) %>%
  arrange(cod_dpto)

message("Geometría procesada correctamente")
message(sprintf("  - Polígonos: %d", nrow(carto)))
message(sprintf("  - Sistema de referencia: %s", st_crs(carto)$input))
