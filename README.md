# Medellín violence mapping

Bayesian space-time analysis of sexual and non-sexual violence notifications
among women and girls in Medellín, Colombia (2018–2022), using R-INLA.

The main model, M4, describes a shared spatial pattern across 285 neighbourhoods
and rural units. Its contribution to each outcome can change before, during and
after the pandemic restrictions. These comparisons are descriptive, not causal.

## Run

With the dependencies and input data in place, run from the repository root:

```r
source("main.R")
```

This prepares data, fits models and runs the existing robustness and results
scripts. **M4 results require additional steps:** see [R/README.md](R/README.md#run).
The drivers clear the R workspace and overwrite generated outputs.

## Structure

- [`main.R`](main.R) — entry point.
- [`R/Data/`](R/Data) — input data.
- [`R/Data_preparation/`](R/Data_preparation) — counts, population and spatial graph.
- [`R/All_Models/`](R/All_Models) — model definitions and fitting.
- [`R/Robustness/`](R/Robustness) — M4 fitting and sensitivity checks.
- [`R/Results/`](R/Results) — posterior summaries, tables and figures.
- [`article_public_health.tex`](article_public_health.tex) — manuscript.
- [`supplementary_material.tex`](supplementary_material.tex) — supplementary source.
- `results/`, `figures/` — generated outputs; gitignored.

## Data

| Input | Location |
|---|---|
| Cleaned cases and raw MEData export | `R/Data/Cases/` — see [file requirements](R/Data/Cases/README.md) |
| Female population projections | `R/Data/Population/proyecciones_barrios_2018_2030.xlsx` |
| Neighbourhood boundaries | `R/Data/Carto/barrios_y_veredas_mr.shp` and sidecars |
| IMCV index for the M4 results | `R/Data/IMCV/imcv_medellin.csv` |

Case microdata are **not distributed**. Contact the project team for authorised
access. Paths and study settings are in [config.R](R/Data_preparation/config.R).

## Dependencies

```r
install.packages(c("dplyr", "tidyr", "stringr", "readr", "readxl", "sf", "spdep",
                   "Matrix", "ggplot2", "scales"))
install.packages("INLA", repos = c(getOption("repos"),
                 INLA = "https://inla.r-inla-download.org/R/stable"),
                 dependencies = TRUE)
```

Recorded environment: R 4.4.2 and INLA 24.12.11. These commands install the
available versions; record `sessionInfo()` when rerunning. PDF export needs Cairo.

## Model reference

Shared-interaction code is adapted from
[Retegui et al. (2024)](https://doi.org/10.1007/s10651-024-00630-w).
Source revision and changes are recorded in the
[implementation header](R/All_Models/inla_rgeneric_scm_change_typeI.R).
