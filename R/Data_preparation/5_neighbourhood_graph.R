################################################################################
# Queen-contiguity graph of the analysis units (islands joined to their nearest
# centroid), the area index used by every model, and the map polygons.
################################################################################
source("R/Data_preparation/config.R")

panel     <- read_csv(file.path(DIR_DATA, "panel.csv"), show_col_types = FALSE)
merge_map <- read_csv(file.path(DIR_DATA, "unit_merge.csv"), show_col_types = FALSE)

carto <- st_read(FILE_CARTO, quiet = TRUE) %>%
  st_make_valid() %>%
  mutate(cod_barrio = normalise_code(codigo)) %>%
  left_join(merge_map, by = "cod_barrio") %>%
  mutate(cod_barrio = coalesce(cod_destino, cod_barrio)) %>%
  group_by(cod_barrio) %>%
  summarise(nombre_shp = first(nombre), .groups = "drop") %>%
  filter(cod_barrio %in% panel$cod_barrio) %>%
  arrange(cod_barrio) %>%
  mutate(id_area = row_number())
stopifnot(setequal(carto$cod_barrio, panel$cod_barrio))

nb <- poly2nb(carto, queen = TRUE)
cent <- st_centroid(st_geometry(carto))
for (i in which(card(nb) == 0)) {
  d <- as.numeric(st_distance(cent[i], cent)); d[i] <- Inf
  j <- which.min(d)
  nb[[i]] <- sort(unique(c(setdiff(nb[[i]], 0L), j)))
  nb[[j]] <- sort(unique(c(setdiff(nb[[j]], 0L), i)))
}
stopifnot(n.comp.nb(nb)$nc == 1)

nb2INLA(file.path(DIR_DATA, "barrios_medellin.graph"), nb)
write_csv(st_drop_geometry(carto)[, c("id_area", "cod_barrio", "nombre_shp")],
          file.path(DIR_DATA, "areas.csv"))
saveRDS(carto, file.path(DIR_DATA, "carto.rds"))
message(sprintf("Graph: %d areas", nrow(carto)))
