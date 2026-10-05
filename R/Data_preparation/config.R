# Paths, study settings and helpers shared by the data-preparation scripts.
# Every script is run from the repository root.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr)
  library(readxl); library(sf); library(spdep); library(INLA)
})

## inputs
FILE_POPULATION <- "R/Data/Population/proyecciones_barrios_2018_2030.xlsx"
FILE_CARTO      <- "R/Data/Carto/barrios_y_veredas_mr.shp"
FILE_CASES      <- "R/Data/Cases/medata_1900_2022_debugged.csv"   # not distributed
FILE_CASES_RAW  <- "R/Data/Cases/medata_1900_2022.csv"   # raw MEData export, for the notifying unit

## outputs
DIR_DATA <- "results/data"
dir.create(DIR_DATA, recursive = TRUE, showWarnings = FALSE)

## study settings
YEARS <- 2018:2022          # 2017 is incomplete in MEData; projections start in 2018
SEXUAL_CODES <- c("4", "5", "6", "7", "10", "12", "14", "15")   # def_naturaleza
MIN_POP_MERGE <- 200        # units below this mean female population are merged
MIN_POP_MODEL <- 10         # units below this are dropped after merging

## MEData neighbourhood codes absent from the cartography, matched by name
REMAP_CODES <- c("6098" = "AE1", "6000" = "AUC1", "7096" = "AE5",
                 "7097" = "AE6", "8098" = "AE7", "8000" = "AUC2")

## perpetrator relationships counted as intrafamilial (parentezco_agresor)
INTRAFAMILIAL <- "Pareja|Familiar|Madre|Padre|Hij|Herman|Abuel|T[íi]o|Primo|Cu[ñn]ad"

AGE_GROUPS <- c(paste(seq(0, 75, 5), seq(4, 79, 5), sep = "-"), "80+")
ADULT_AGES <- AGE_GROUPS[-(1:4)]   # 20 and over; the 15-19 band does not split at 18

normalise_code <- function(x) {
  x <- str_trim(as.character(x))
  ifelse(str_detect(x, "^[0-9]+$"), str_pad(x, 4, pad = "0"), x)
}

age_group <- function(age) {
  age <- suppressWarnings(as.numeric(age))
  as.character(cut(age, breaks = c(seq(0, 80, 5), Inf), labels = AGE_GROUPS,
                   right = FALSE, include.lowest = TRUE))
}
