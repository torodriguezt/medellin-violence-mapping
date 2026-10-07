# R analysis pipeline

Code for the results in `article_public_health.tex`, including its appendices.
M4 is the main model. Exploratory models and superseded M3 analyses are excluded.

## Run

From the repository root in a fresh R session:

```r
source("main.R")
```

Or run the four stages in order:

```r
source("R/Data_preparation/Data_preparation.R")
source("R/All_Models/All_Models.R")
source("R/Robustness/Robustness.R")
source("R/Results/Results.R")
```

Drivers clear the workspace and overwrite outputs. The full run can take hours:
it includes appendix refits and 5,000 continuous-integration draws. Completed
sampling batches are reused; a changed draw bank is archived automatically.

## Structure

| Folder | Purpose |
|---|---|
| [Data_preparation/](Data_preparation) | Counts, population, expected counts and spatial graph |
| [All_Models/](All_Models) | The eight models in the comparison table, including M4 |
| [Robustness/](Robustness) | Published M4 sensitivity, holdout and appendix identification results |
| [Results/](Results) | Article tables, descriptive figures and M4 figures |

M4 is fitted by [model_M4.R](All_Models/model_M4.R). Its saved fit keeps the
historical filename `results/fits/M3_l3_both_loadings_all.rds`.

## Tables

| Article result | Script | CSV under `results/tables/` |
|---|---|---|
| Model comparison, fitting times, M4 parameters and hotspots | [Results_M4.R](Results/Results_M4.R) | `M4/model_comparison.csv`, `M4/cpu_times.csv`, `M4/parameters.csv`, `M4/hotspots.csv` |
| M4 period loadings and contrasts | [Table_M4_loadings.R](Results/Table_M4_loadings.R) | `M4/period_loadings.csv` |
| M4 sensitivity | [M4_sensitivity.R](Robustness/M4_sensitivity.R) | `M4/sensitivity.csv` |
| Starting values and identification | [M4_starting_values.R](Robustness/M4_starting_values.R), [M4_phi2_profile.R](Robustness/M4_phi2_profile.R) | `M4_starts/all.csv`, `all_restart.csv`, `phi2_profile.csv` |
| M3 period and annual loadings | [Table_period_loadings.R](Results/Table_period_loadings.R), [Table_annual_loadings.R](Results/Table_annual_loadings.R) | `period_loadings.csv`, `annual_loadings.csv` |
| Shared-field integration comparison | [psi_continuous_M4.R](Results/psi_continuous_M4.R), [Results_M4.R](Results/Results_M4.R) | `M4/risk_contrasts.csv` |

The methodological and commune-name tables are defined directly in the LaTeX
article. Figures go to `figures/` and `figures/M4/`. M3 posterior sampling is
retained only for the M3/M4 agreement reported in the text.

## Data

See the [input list](../README.md#data) and [case-file requirements](Data/Cases/README.md).
The raw case export is required for health-only analyses. Settings are in
[config.R](Data_preparation/config.R); several M4 scripts assume 285 units and 2018?2022.

## Dependencies

Use the [installation commands](../README.md#dependencies) in the project README.
Refitting with other software versions can change numerical results and timings.

## Model reference

See the [project model reference](../README.md#model-reference).
