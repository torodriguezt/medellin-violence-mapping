# R analysis pipeline

Scripts for data preparation, model fitting and results. M4 is the main model;
M3 and the other specifications provide comparisons.

## Run

Start a fresh R session at the repository root:

```r
source("main.R")
```

This runs the four drivers listed below, including the M4 fit. Most default
results still use M3. **For M4 results, continue in this order:**

```r
rm(list = ls()); source("R/Results/posterior_draws_M4.R")
rm(list = ls()); source("R/Robustness/holdout_M4.R")
rm(list = ls()); source("R/Robustness/geocoding_coded_only_M4.R")
rm(list = ls()); source("R/Results/psi_continuous_M4.R")
rm(list = ls()); source("R/Results/Results_M4.R")
rm(list = ls()); source("R/Results/Figures_M4.R")
```

Additional M4 checks, run separately:

```r
rm(list = ls()); source("R/Robustness/M4_starting_values.R")
rm(list = ls()); source("R/Robustness/M4_phi2_profile.R")
rm(list = ls()); source("R/Robustness/likelihood_negbin_M4.R")
```

Drivers clear the workspace and overwrite outputs. Continuous integration reuses
saved batches; use a fresh batch directory if the fit or sampling settings change.

## Structure

| Driver | Purpose | Output |
|---|---|---|
| [Data_preparation.R](Data_preparation/Data_preparation.R) | Population, cases, expected counts and spatial graph | `results/data/` |
| [All_Models.R](All_Models/All_Models.R) | Comparison-model fits | `results/fits/` |
| [Robustness.R](Robustness/Robustness.R) | M3 sensitivity checks and M4 fits | `results/fits/`, `results/tables/` |
| [Results.R](Results/Results.R) | Mainly M3 summaries and plots | `results/posterior/`, `results/tables/`, `figures/` |

M4 is fitted by [loadings_both_outcomes.R](Robustness/loadings_both_outcomes.R)
and saved as `results/fits/M3_l3_both_loadings_all.rds`. Its additional tables
and figures go to `results/tables/M4/` and `figures/M4/`.

M2-II and M2-IV are disabled in the model driver because earlier fits retained
numerical warnings. M5 is exploratory.

## Data

See the [input list](../README.md#data) and [case-file requirements](Data/Cases/README.md).
Settings are in [config.R](Data_preparation/config.R). The raw case export is
required for health-only analyses; IMCV is used for a descriptive comparison in
M4 results. Several M4 scripts assume 285 units and the years 2018–2022.

## Dependencies

Use the [installation commands](../README.md#dependencies) in the project README.
Check numerical diagnostics before using regenerated manuscript results: the
comparison scripts retain flagged CPO values without the manuscript's later review.

## Model reference

See the [project model reference](../README.md#model-reference).
