# R analysis pipeline

Code for the bivariate analysis of violence notifications in Medellín, 2018–2022.
The main specification is M3 with three relative spatial loadings: pre-pandemic,
restriction and post-restriction. See the [project README](../README.md) for the
study description and package installation.

## Run

Run from the repository root with the input data in place, using a fresh R session:

```r
source("main.R")
```

Alternatively, run the four stages in order:

```r
source("R/Data_preparation/Data_preparation.R")
source("R/All_Models/All_Models.R")
source("R/Robustness/Robustness.R")
source("R/Results/Results.R")
```

The drivers clear the workspace between scripts and rerun the fits, overwriting
their saved outputs. Later stages read the files written by earlier ones.
To run one model separately after data preparation:

```r
source("R/All_Models/model_M3_l3.R")
```

## Data

- [`Data/Population/`](Data/Population) — female population projections, 2018–2030.
- [`Data/Carto/`](Data/Carto) — neighbourhood and rural-unit shapefile and sidecars.
- [`Data/Cases/`](Data/Cases) — local cleaned case file; microdata are not distributed.

Input paths and study settings are in
[`Data_preparation/config.R`](Data_preparation/config.R).

## Data preparation

Driver: [`Data_preparation.R`](Data_preparation/Data_preparation.R).
Outputs: `results/data/`.

| Script | Purpose |
|---|---|
| `config.R` | Paths, years, violence codes, relationship classification and helpers |
| `1_population.R` | Female population by neighbourhood, year and five-year age group |
| `2_cases.R` | Case counts by area, year, outcome, age group and relationship; reads the cleaned microdata |
| `3_merge_units.R` | Merge institutional, unnamed and low-population units into eligible neighbours, using shared border length or a distance fallback |
| `4_expected_counts.R` | Observed and age-standardised expected counts; also writes age-specific cells for the holdout check |
| `5_neighbourhood_graph.R` | Queen-contiguity graph, area index and map polygons |

The study panel has 285 analysis units, five years and two outcomes (2,850 cells).
The code derives the units from the supplied population and cartography rather
than hard-coding that count. Data preparation starts from an already cleaned case
file; it does not recreate the earlier microdata cleaning and name matching.

## Models

Driver: [`All_Models.R`](All_Models/All_Models.R).
Outputs: one fit per model in `results/fits/`.
[`model_data.R`](All_Models/model_data.R) defines shared inputs, indices, priors,
constraints and the fitting helper.

| Script | Model |
|---|---|
| `model_M0.R` | Outcome-specific spatial fields and temporal trends with separate between-outcome correlations |
| `model_M1.R` | Shared spatial and temporal components with constant loadings |
| `model_M2_I.R` to `model_M2_IV.R` | M1 plus a Knorr-Held interaction of Type I, II, III or IV |
| `model_M3_annual.R` | Type I interaction and one relative spatial loading per year (`l = T`) |
| `model_M3_l3.R` | Type I interaction and three period-specific spatial loadings (`l = 3`); main model |
| `model_SCM.R` | Shared-interaction model with time-varying interaction loadings |
| `model_M5.R` | Combined spatial and interaction loadings; exploratory |
| `inla_rgeneric_scm_change_typeI.R` | Shared-interaction implementation adapted from Retegui et al. (2024), with provenance in its header |

The default driver runs all of these except M2-II and M2-IV. Those two scripts
remain available for separate execution; earlier fits retained numerical warnings.
Runtime depends on the model, software and hardware.

## Robustness

Driver: [`Robustness.R`](Robustness/Robustness.R).
Refits of M3 (`l = 3`), saved in `results/fits/`.

| Script | Change |
|---|---|
| `prior_N04.R`, `prior_N11.R` | Normal loading priors with mean/variance (0, 4) and (1, 1) |
| `spatial_ICAR.R` | Scaled intrinsic CAR in place of the BYM2 spatial fields |
| `stratum_intrafamilial.R`, `stratum_complement.R` | Intrafamilial notifications and their complement, including unknown or missing relationships |
| `holdout.R` | Random 15% holdout of area-year-outcome cells, with expected counts rebuilt from training cells |

The holdout checks prediction within the observed areas and years. It is not a
forecast or validation in new areas.

## Results

Driver: [`Results.R`](Results/Results.R).
Outputs: posterior draws in `results/posterior/`, CSV tables in `results/tables/`
and PNG/PDF plots in `figures/`.

| Script | Output |
|---|---|
| `posterior_draws.R` | 2,000 joint posterior draws of M3 (`l = 3`) for risk and temporal summaries and predictive checks |
| `Table_model_comparison.R` | WAIC, DIC, native CPO-based LS, WAIC effective parameter count, flagged CPO count and recorded fitting time |
| `Table_period_loadings.R` | Period loadings and contrasts, both summarised from 5,000 joint hyperparameter draws |
| `Table_annual_loadings.R` | Annual loadings and contrasts against the within-model pre-pandemic mean |
| `Table_parameters.R` | Parameters and hyperparameters of M3 (`l = 3`) |
| `Table_sensitivity.R` | Period contrasts under prior, spatial-prior and relationship-stratified refits |
| `Table_hotspots.R` | Joint exceedance and persistent hotspots, using 1,000 selected joint draws |
| `Figures_descriptive.R` | Study region, crude-rate maps and trends |
| `Figures_risk.R` | Relative risks, selected neighbourhoods, spatial components and hotspots |
| `Figures_loadings.R` | Period loadings and temporal factors |
| `Figures_checks.R` | PIT, posterior predictive check and cell-level holdout plots |
| `figure_style.R` | Plot colours, themes and export helper |

The model-comparison script includes any listed model whose saved fit exists,
including separately fitted M2-II, M2-IV or exploratory M5. Its LS retains native
CPO values, including flagged cells; it does not perform the subsequent per-cell
numerical review used for the manuscript's descriptive LS comparison. The
shared-field hyperparameter-integration sensitivity supplement also requires
postprocessing beyond this driver. These outputs therefore need those additional
checks before being substituted for the reviewed manuscript results.

The period-loading CSV summarises individual loadings from draws, whereas the
parameter table and loading figure use INLA's integrated marginals. Keep that
distinction when preparing manuscript tables. The 5,000 hyperparameter draws used
for loading contrasts and the 2,000 joint draws of predictors and temporal effects
are separate calculations. The hotspot rule selects areas with joint exceedance
in every selected draw in every year; it does not establish a posterior probability
exactly equal to one.
