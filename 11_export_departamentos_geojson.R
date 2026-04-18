#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
})

in_shp <- "SHP_MGN2018_INTGRD_DEPTO/MGN_ANM_DPTOS.shp"
out_geo <- "departamentos.geojson"

if (!file.exists(in_shp)) {
  stop(sprintf("Shapefile not found at '%s'", in_shp))
}

depto <- st_read(in_shp, quiet = TRUE)

# Write as GeoJSON for web usage
st_write(depto, out_geo, quiet = TRUE)
message(sprintf("Wrote GeoJSON: %s", out_geo))
