# =====================================================
# 04_vecindad_grafo.R
# Construcción del grafo de vecindad para INLA
# =====================================================

source("03_preparar_datos_modelo.R")

# --- Grafo de vecindad (Queen) ---
message("Construyendo grafo de vecindad (Queen)...")
nb0 <- poly2nb(as_Spatial(depto_agg_sf), queen = TRUE, snap = 1e-08)

# Inspección de la matriz de vecindad
W <- nb2mat(nb0, style = "B", zero.policy = TRUE)
message(sprintf("  - Conexiones totales: %d", sum(W)))
message(sprintf("  - Promedio de vecinos: %.2f", mean(card(nb0))))

# Exportar a formato INLA
tmp_graph <- tempfile(fileext = ".graph")
nb2INLA(file = tmp_graph, nb = nb0)
g <- INLA::inla.read.graph(tmp_graph)

# --- Grafo alternativo (Rook) para análisis de sensibilidad ---
message("Construyendo grafo alternativo (Rook)...")
nb_rook <- poly2nb(as_Spatial(depto_agg_sf), queen = FALSE)
tmp_g2 <- tempfile(fileext = ".graph")
nb2INLA(tmp_g2, nb_rook)
g2 <- INLA::inla.read.graph(tmp_g2)

# --- Lista ponderada para pruebas de Moran ---
listw <- nb2listw(nb0, style = "W", zero.policy = TRUE)

message("Grafos de vecindad construidos")
message(sprintf("  - Queen: %d vecindades", sum(card(nb0))))
message(sprintf("  - Rook: %d vecindades", sum(card(nb_rook))))
