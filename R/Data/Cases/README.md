# Case data

Cleaned MEData/SIVIGILA notifications of gender-based and intrafamilial violence
(INS 875 form). These records contain sensitive health information and are
**not included** in the repository. Contact the project team about access to an
authorised extract.

## Input

Place the UTF-8 CSV file here:

```text
R/Data/Cases/medata_1900_2022_debugged.csv
```

The pipeline starts from this already cleaned extract. It does not recreate the
earlier cleaning or the name matching that produced `cod_barrio_nuevo`.
This folder is gitignored except for this README.

## Required columns

[`2_cases.R`](../../Data_preparation/2_cases.R) reads the following columns.
Keep the names and spelling exactly as shown.

| Column | Use |
|---|---|
| `año_not` | Notification year; keep 2018–2022 |
| `sexo_` | Victim's sex; keep `F`, including girls and adolescents |
| `edad` | Age in years; grouped into five-year bands and 80+ |
| `codigo_barrio` | Preprocessed residential-neighbourhood code, used preferentially |
| `cod_barrio_nuevo` | Previously assigned code from name matching, used as a fallback |
| `def_naturaleza` | Codes 4, 5, 6, 7, 10, 12, 14 and 15 identify sexual violence; the script assigns other values to the non-sexual category |
| `parentezco_agresor` | Relationship text used for the intrafamilial indicator; unmatched, unknown and missing values fall in the complement |

## Geographic assignment

The script normalises codes and uses `codigo_barrio` when it has four digits and
is not `0000`; otherwise it uses `cod_barrio_nuevo`. Code remapping and subsequent
unit merging are defined in the data-preparation scripts. This format check does
not independently validate an address.

Fallback assignments were prepared using neighbourhood names and may refer to
residence or place of occurrence. They should not all be interpreted as verified
residential locations, especially when used with resident population denominators.

## Run

After placing the file and the population/cartography inputs, run from the
repository root:

```r
source("R/Data_preparation/Data_preparation.R")
```

The case step writes aggregated counts to `results/data/cases.csv`; later steps
merge units and construct the modelling panel and expected counts. For the full
analysis, use `source("main.R")` as described in the [project README](../../../README.md).
