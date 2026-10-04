################################################################################
# Merge institutional, unnamed and low-population cartographic units into the
# valid neighbour with which they share the longest border.
################################################################################
source("R/Data_preparation/config.R")

pop <- read_csv(file.path(DIR_DATA, "population.csv"), show_col_types = FALSE)
mean_pop <- pop %>% group_by(cod_barrio) %>% summarise(pop = mean(pop), .groups = "drop")

carto <- st_read(FILE_CARTO, quiet = TRUE) %>%
  st_make_valid() %>%
  mutate(cod_barrio = normalise_code(codigo)) %>%
  group_by(cod_barrio) %>%
  summarise(nombre = first(nombre), .groups = "drop") %>%
  left_join(mean_pop, by = "cod_barrio") %>%
  mutate(pop = replace_na(pop, 0),
         merge = str_detect(cod_barrio, "^Inst") | str_detect(cod_barrio, "^SN") |
                 pop < MIN_POP_MERGE)

border_length <- function(i, j) {
  inter <- suppressWarnings(st_intersection(st_geometry(carto)[i], st_geometry(carto)[j]))
  if (length(inter) == 0) return(0)
  len <- suppressWarnings(try(sum(as.numeric(st_length(
    st_collection_extract(inter, "LINESTRING")))), silent = TRUE))
  if (inherits(len, "try-error") || !is.finite(len))
    len <- suppressWarnings(try(sum(as.numeric(st_length(inter))), silent = TRUE))
  if (inherits(len, "try-error") || !is.finite(len)) 0 else len
}

valid <- which(!carto$merge)
nb    <- poly2nb(carto, queen = TRUE)
cent  <- suppressWarnings(st_point_on_surface(st_geometry(carto)))
destination <- carto$cod_barrio

for (i in which(carto$merge)) {
  neighbours <- intersect(setdiff(nb[[i]], 0L), valid)
  j <- if (length(neighbours) > 0) {
    neighbours[which.max(vapply(neighbours, function(j) border_length(i, j), numeric(1)))]
  } else {
    valid[which.min(as.numeric(st_distance(cent[i], cent[valid])))]
  }
  destination[i] <- carto$cod_barrio[j]
}

merge_map <- tibble(cod_barrio = carto$cod_barrio, cod_destino = destination)
write_csv(merge_map, file.path(DIR_DATA, "unit_merge.csv"))
message(sprintf("Merged %d of %d units -> %d units", sum(carto$merge), nrow(carto),
                n_distinct(destination)))
