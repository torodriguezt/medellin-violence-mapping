# Modelo Orquidea

Code for a Bayesian bivariate space-time model (R-INLA) of sexual and non-sexual
violence against women in Medellín, Colombia (2018-2022).

The model fits a shared BYM2 spatial field across 318 neighbourhoods, with a
dynamic loading `delta_t` that lets the spatial coupling between the two
violence types vary by period (pre-lockdown / lockdown / post-lockdown) —
testing whether the 2020 COVID-19 lockdown changed how the two forms of
violence cluster together in space.

## Run

```r
source("main.R")
```

Runs the core pipeline (population, cases, expected counts, neighbourhood
graph, model M3, covariates, joint exceedance, figures). Additional scripts
in `scripts/` (10-30) run forecasts, publication figures/maps, and robustness
checks; run them individually after `main.R`.

## Structure

- `scripts/` — pipeline, numbered in run order (`00_config.R` first)
- `results/` — model fits and tables (generated; `.rds` fits are gitignored)
- `figures/` — plots (generated)
- `datos/` — input data

## Data

Raw case microdata (MEData/SIVIGILA) contain sensitive health information and
are not included; request from the project team. Population projections and
shapefiles are public (Alcaldía de Medellín, DANE).

## Dependencies

```r
install.packages(c("sf", "dplyr", "tidyr", "stringr", "stringi", "tibble",
                   "readr", "readxl", "spdep", "ggplot2", "scales", "viridis"))
install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"))
```
