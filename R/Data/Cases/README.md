# Case data

MEData/SIVIGILA violence notifications used for the analysis of women and girls
in Medellín, 2018–2022. Microdata are **not distributed**; contact the project
team for authorised access.

## Run

Place the files below in this folder, then run from the repository root:

```r
source("R/Data_preparation/Data_preparation.R")
```

Population and cartography inputs must also be present. Aggregated case counts
are written to `results/data/cases.csv`.

## Structure

- `medata_1900_2022_debugged.csv` — cleaned cases, required for all analyses.
- `medata_1900_2022.csv` — raw export, required for health-only analyses.
- `README.md` — this guide; all other files in the folder are gitignored.

## Data

Use UTF-8 CSV files and keep the original column names. The main case counts use:

| Column | Content |
|---|---|
| `año_not` | Notification year |
| `sexo_` | Victim's sex; the analysis keeps `F` |
| `edad` | Age in years |
| `codigo_barrio` | Original neighbourhood code |
| `cod_barrio_nuevo` | Fallback code assigned by name |
| `def_naturaleza` | Type of violence |
| `parentezco_agresor` | Relationship to the aggressor |

The pipeline expects an already cleaned extract. Fallback locations may refer
to residence or place of occurrence.

Health-only analyses link the cleaned file to `nom_upgd` in the raw export.
Keep the full extracts: linkage requires additional date, age and other fields,
listed in [link_notifier()](../../Data_preparation/2_cases.R). Unmatched records
are excluded from health-only analyses.

## Dependencies

See the [project README](../../../README.md) for packages and other inputs,
and [config.R](../../Data_preparation/config.R) for file paths.
