# =====================================================
# 00_config.R
# Configuración global y rutas del proyecto
# =====================================================

# --- Librerías ---
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

# --- Configuración INLA ---
INLA::inla.setOption(num.threads = "2:2")  # Ajusta según tu máquina

# --- Rutas de datos ---
RUTAS <- list(
  sivigila = "SIVIGILA_debugged_v2.rds",
  shapefile = "SHP_MGN2018_INTGRD_DEPTO/MGN_ANM_DPTOS.shp"
)

# --- Funciones auxiliares ---
# Formato códigos DANE a 2 dígitos
fmt2 <- function(x) sprintf("%02d", as.integer(x))

# Transformación logit inversa
ilogit <- function(x) 1 / (1 + exp(-x))

# --- Nombres de columnas del shapefile ---
COLUMNAS_SHAPEFILE <- list(
  candidatos_codigo = c("DPTO_CCDGO", "COD_DEPTO", "COD_DANE", "DPTO_CCDGO", "COD_DPTO", "DPTO"),
  candidatos_nombre = c("DPTO_CNMBR", "NOMBRE_DPT", "NOMBRE", "NOMBRE_DEP", "DPTO_CNMBR")
)

# --- Parámetros de simulación ---
NSIM <- 3000L  # Número de simulaciones para predicción posterior
SEED <- 123    # Semilla para reproducibilidad

message("Configuración cargada correctamente")
