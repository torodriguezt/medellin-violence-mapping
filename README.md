# Medellín violence mapping

Code for Bayesian bivariate space-time models (R-INLA) of sexual and non-sexual
violence notifications among women and girls in Medellín, Colombia (2018–2022).

The main model, M3, uses a shared BYM2 spatial field across 285 analysis units
(243 urban neighbourhoods and 42 rural units). A period-specific loading
`delta_t` describes how strongly this shared pattern contributes to sexual-violence
log-risk relative to non-sexual violence in the pre-pandemic (2018–2019),
restriction (2020–2021) and post-restriction (2022) periods. The analysis compares
notification patterns over time; these contrasts do not identify a causal effect
of restrictions or a change in correlation between the full risk maps.

## Run

Install the dependencies below and place the authorised cleaned case file in
`R/Data/Cases/`. Then run from the repository root in a fresh R session:

```r
source("main.R")
```

Runs data preparation, model fitting, robustness checks and results generation:
population, case counts, age-standardised expected counts, neighbourhood graph,
models, posterior summaries, joint exceedance probabilities, tables and figures.
The drivers clear the R workspace between scripts. Individual stages can also be
run in order; see [`R/README.md`](R/README.md).

The default model driver includes M0, M1, M2-I, M2-III, both versions of M3, SCM
and exploratory M5. M2-II and M2-IV are available separately and are disabled in
the driver because their earlier fits retained numerical warnings.

## Structure

- [`main.R`](main.R) — entry point for the four stages.
- [`R/Data/`](R/Data) — population, cartography and the local case-file location.
- [`R/Data_preparation/`](R/Data_preparation) — population, cases, unit merging,
  expected counts and neighbourhood graph.
- [`R/All_Models/`](R/All_Models) — model specifications and fitting scripts.
- [`R/Robustness/`](R/Robustness) — loading-prior and spatial-prior sensitivity,
  relationship strata and cell-level holdout checks.
- [`R/Results/`](R/Results) — posterior draws, tables and figures.
- `results/` — generated data, fits, posterior draws and tables; gitignored.
- `figures/` — generated PNG and PDF plots; gitignored.

## Data

Case microdata from MEData/SIVIGILA (INS 875 notifications) contain sensitive
health information and are **not included**. Contact the project team about
access to the authorised cleaned extract. The required filename and columns are
documented in [`R/Data/Cases/README.md`](R/Data/Cases/README.md).

The pipeline also expects the municipal female population projections in
`R/Data/Population/proyecciones_barrios_2018_2030.xlsx` and the neighbourhood and
rural-unit shapefile in `R/Data/Carto/`. Input paths and study settings are defined
in [`R/Data_preparation/config.R`](R/Data_preparation/config.R).

## Dependencies

```r
install.packages(c("dplyr", "tidyr", "stringr", "readr", "readxl", "sf", "spdep",
                   "Matrix", "ggplot2", "scales"))
install.packages("INLA", repos = c(getOption("repos"),
                 INLA = "https://inla.r-inla-download.org/R/stable"),
                 dependencies = TRUE)
```

The INLA command follows the [official installation instructions](https://www.r-inla.org/download/).
It installs the available stable release rather than pinning the original
environment. PDF export uses R's Cairo device (`capabilities("cairo")`).
The original analysis records R 4.4.2 and INLA 24.12.11; record
`sessionInfo()` when rerunning and check numerical diagnostics before using new
outputs. The scope of the current results scripts, including the additional
postprocessing needed for the manuscript, is described in
[`R/README.md`](R/README.md#results).

## Model reference

The shared-interaction models use the `rgeneric` implementation from
[spatialstatisticsupna/Shared_interactions](https://github.com/spatialstatisticsupna/Shared_interactions).
Its source revision and namespace adjustments are documented in
[`inla_rgeneric_scm_change_typeI.R`](R/All_Models/inla_rgeneric_scm_change_typeI.R).

Retegui, G., Etxeberria, J. and Ugarte, M.D. (2024). Multivariate Bayesian models
with flexible shared interactions for analyzing spatio-temporal patterns of rare
cancers. *Environmental and Ecological Statistics*.
[doi:10.1007/s10651-024-00630-w](https://doi.org/10.1007/s10651-024-00630-w).

Identifiability constraints are motivated by Goicoa, T., Adin, A., Ugarte, M.D.
and Hodges, J.S. (2018). In spatio-temporal disease mapping models, identifiability
constraints affect PQL and INLA results. *Stochastic Environmental Research and
Risk Assessment*, 32, 749–770.
