################################################################################
# Cases of violence against women by neighbourhood, year, type, age group and
# perpetrator relationship. This is the only script that reads the microdata.
################################################################################
source("R/Data_preparation/config.R")

medata <- read_csv(FILE_CASES, show_col_types = FALSE,
                   locale = locale(encoding = "UTF-8"))

cases <- medata %>%
  mutate(
    year = as.integer(.data[["año_not"]]),
    # keep the original residence code when valid; cod_barrio_nuevo was
    # assigned by name and misplaces cases whose name repeats across communes
    code_orig = normalise_code(codigo_barrio),
    code_new  = normalise_code(cod_barrio_nuevo),
    orig_ok   = coalesce(str_detect(code_orig, "^[0-9]{4}$") & code_orig != "0000", FALSE),
    cod_barrio = ifelse(orig_ok, code_orig, code_new),
    cod_barrio = ifelse(cod_barrio %in% names(REMAP_CODES),
                        REMAP_CODES[cod_barrio], cod_barrio),
    type = ifelse(as.character(def_naturaleza) %in% SEXUAL_CODES, "sexual", "non_sexual"),
    age  = age_group(edad),
    intrafamilial = str_detect(coalesce(parentezco_agresor, ""), INTRAFAMILIAL)
  ) %>%
  filter(year %in% YEARS, sexo_ == "F", !is.na(cod_barrio), cod_barrio != "00NA")

counts <- cases %>% count(cod_barrio, year, type, age, intrafamilial, name = "y")

write_csv(counts, file.path(DIR_DATA, "cases.csv"))
message(sprintf("Cases: %d women, %d neighbourhood codes, %.1f%% intrafamilial",
                nrow(cases), n_distinct(cases$cod_barrio), 100 * mean(cases$intrafamilial)))
