# =====================================================
# 03_preparar_datos_modelo.R
# Preparación de tabla de análisis con casos y población
# =====================================================

source("02_procesar_geometria.R")

# --- Unión de datos ---
message("Uniendo casos, población y geometría...")
df_base <- pop_fem_depto %>%
  select(cod_dpto, depto, pop_fem) %>%
  left_join(y_depto, by = "cod_dpto") %>%
  mutate(y_obs = replace_na(y_obs, 0L)) %>%  # Departamentos sin casos
  arrange(cod_dpto)

# --- Validación de consistencia ---
faltan_en_mapa <- anti_join(df_base, carto %>% st_drop_geometry() %>% select(cod_dpto), by = "cod_dpto")
sobran_en_mapa <- anti_join(carto %>% st_drop_geometry() %>% select(cod_dpto), df_base, by = "cod_dpto")

if (nrow(faltan_en_mapa) > 0) {
  stop("Códigos en población sin geometría: ", paste(faltan_en_mapa$cod_dpto, collapse=", "))
}
if (nrow(sobran_en_mapa) > 0) {
  message("Aviso: códigos en shapefile sin población: ", paste(sobran_en_mapa$cod_dpto, collapse=", "))
}

# --- Tabla final con geometría ---
message("Construyendo tabla de análisis...")
depto_agg_sf <- carto %>%
  inner_join(df_base, by = "cod_dpto") %>%
  arrange(cod_dpto) %>%
  mutate(
    id        = row_number(),
    y_model   = y_obs,
    E_pop     = pop_fem,
    E_classic = pop_fem * (sum(y_obs) / sum(pop_fem))  
  ) %>%
  relocate(id, cod_dpto, depto, y_obs, y_model, pop_fem, E_pop, E_classic)

# --- Tabla para INLA (sin geometría) ---
agg_depto_table <- depto_agg_sf %>%
  st_drop_geometry() %>%
  select(id, cod_dpto, depto, y_obs, y_model, pop_fem, E_pop, E_classic)

message("Tabla de análisis preparada")
message(sprintf("  - Departamentos en análisis: %d", nrow(agg_depto_table)))
message(sprintf("  - Total casos: %d", sum(agg_depto_table$y_obs)))
message(sprintf("  - Población total: %s", format(sum(agg_depto_table$pop_fem), big.mark=",")))
message(sprintf("  - Tasa cruda nacional: %.2f por 100,000", 
                100000 * sum(agg_depto_table$y_obs) / sum(agg_depto_table$pop_fem)))
