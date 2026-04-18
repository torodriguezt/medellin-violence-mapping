# =====================================================
# main.R
# Script principal para ejecutar todo el análisis
# =====================================================

# Este script ejecuta el análisis completo de forma secuencial
# Puedes ejecutarlo completo o comentar secciones según necesites

cat("\n")
cat("========================================\n")
cat("ANÁLISIS ESPACIAL - MODELOS INLA BYM2\n")
cat("========================================\n\n")

# --- Paso 1: Configuración ---
cat(">>> PASO 1/9: Cargando configuración...\n")
source("00_config.R")

# --- Paso 2: Cargar datos ---
cat("\n>>> PASO 2/9: Cargando datos...\n")
source("01_cargar_datos.R")

# --- Paso 3: Procesar geometría ---
cat("\n>>> PASO 3/9: Procesando geometría...\n")
source("02_procesar_geometria.R")

# --- Paso 4: Preparar datos del modelo ---
cat("\n>>> PASO 4/9: Preparando datos del modelo...\n")
source("03_preparar_datos_modelo.R")

# --- Paso 5: Construir grafo de vecindad ---
cat("\n>>> PASO 5/9: Construyendo grafo de vecindad...\n")
source("04_vecindad_grafo.R")

# --- Paso 6: Ajustar modelos básicos ---
cat("\n>>> PASO 6/9: Ajustando modelos básicos...\n")
source("05_ajustar_modelos_basicos.R")

# --- Paso 7: Búsqueda de priors ---
cat("\n>>> PASO 7/9: Búsqueda de mejores priors (esto puede tardar)...\n")
source("06_busqueda_priors.R")

# --- Paso 8: Análisis de sensibilidad ---
cat("\n>>> PASO 8/9: Análisis de sensibilidad...\n")
source("07_analisis_sensibilidad.R")

# --- Paso 9: Generar visualizaciones ---
cat("\n>>> PASO 9/9: Generando visualizaciones...\n")
source("08_visualizaciones.R")

# --- Paso 10: Resultados finales ---
cat("\n>>> PASO 10/9: Compilando resultados finales...\n")
source("09_resultados_finales.R")

cat("\n")
cat("========================================\n")
cat("ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("========================================\n")
cat("\nLos objetos principales disponibles son:\n")
cat("  - depto_agg_sf: Datos espaciales con geometría\n")
cat("  - agg_depto_table: Tabla de análisis\n")
cat("  - fit_pois, fit_zip: Modelos básicos\n")
cat("  - best_fit: Mejor modelo según grid de priors\n")
cat("  - rr_zip_rank: Ranking de riesgo relativo\n")
cat("  - p_rr_pois, p_rr_zip, p_best: Mapas de RR\n")
cat("\n")
