# ============================
# Datos
# ============================
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

# ============================================================
# 1. Cargar objeto depto_agg_sf
# ============================================================

depto_agg_sf <- depto_agg_sf %>%
  mutate(
    tasa_cruda = (y_obs / pop_fem) * 100000
  )


data <- data %>%
  mutate(
    depto_norm = canonize(departamento_ocurrencia),
    grupo_bin = if_else(
      def_naturaleza %in% c(1, 2, 3),  # no sexuales
      0L,
      1L                               # sexuales (5,6,7,10,12,14,15,...)
    )
  )

poblacion <- poblacion %>%
  mutate(depto_norm = canonize(dpnom))

poblacion_sumada <- poblacion %>%
  group_by(depto_norm) %>%
  summarise(pop_fem = sum(total_mujeres, na.rm = TRUE), .groups = "drop")

violencia_por_depto <- data %>%
  group_by(depto_norm, grupo_bin) %>%
  summarise(n = n(), .groups = "drop")

final <- violencia_por_depto %>%
  left_join(poblacion_sumada, by = "depto_norm") %>%
  mutate(tasa_por_100k = (n / pop_fem) * 100000)


library(dplyr)

top5_non_sexual <- final %>%
  filter(grupo_bin == 0) %>%
  arrange(desc(tasa_por_100k)) %>%
  slice_head(n = 5)

top5_sexual <- final %>%
  filter(grupo_bin == 1) %>%
  arrange(desc(tasa_por_100k)) %>%
  slice_head(n = 5)


write.csv(final, "tasas_violencia_por_depto.csv", row.names = FALSE)


# ============================================================
# 2. Mapa de tasas crudas
# ============================================================

ggplot(depto_agg_sf) +
  geom_sf(aes(fill = tasa_cruda), color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option = "cividis",
    name = "Crude rate\nper 100,000 women",
    na.value = "grey85"
  ) +
  labs(
    title = "Crude Rate of Gender-Based Violence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )


library(sf)
library(dplyr)
library(ggplot2)

# Reemplazar nombre largo por uno más corto
shp_labels <- shp %>%
  mutate(
    DPTO_CNMBR = case_when(
      DPTO_CNMBR == "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA" ~ 
        "San Andrés y Providencia",
      TRUE ~ DPTO_CNMBR
    )
  )

# Calcular centroides para colocar etiquetas
# (st_point_on_surface garantiza que esté dentro del polígono)
shp_centroids <- shp_labels %>%
  st_point_on_surface()

# Crear el mapa
ggplot() +
  geom_sf(data = shp_labels, fill = "gray95", color = "gray40", size = 0.3) +
  geom_sf_text(data = shp_centroids, aes(label = DPTO_CNMBR), size = 2.2) +
  labs(
    title = "Political Division of Colombia",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

