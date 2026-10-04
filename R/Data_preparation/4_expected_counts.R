################################################################################
# Modelling panel: neighbourhood x year x type, with observed counts and
# expected counts from indirect age standardisation,
#   E_ikt = sum_a N_iat * w_ak,  w_ak = sum_it Y_iakt / sum_it N_iat.
################################################################################
source("R/Data_preparation/config.R")

merge_map <- read_csv(file.path(DIR_DATA, "unit_merge.csv"), show_col_types = FALSE)
apply_merge <- function(df) {
  df %>% left_join(merge_map, by = "cod_barrio") %>%
    mutate(cod_barrio = coalesce(cod_destino, cod_barrio)) %>% select(-cod_destino)
}

pop <- read_csv(file.path(DIR_DATA, "population.csv"), show_col_types = FALSE)
names_ref <- pop %>% distinct(cod_barrio, cod_comuna, nombre_comuna, nombre_barrio)

pop <- pop %>% apply_merge() %>%
  group_by(cod_barrio, year) %>% summarise(pop = sum(pop), .groups = "drop") %>%
  left_join(names_ref, by = "cod_barrio")
pop_age <- read_csv(file.path(DIR_DATA, "population_age.csv"), show_col_types = FALSE) %>%
  apply_merge() %>%
  group_by(cod_barrio, year, age) %>% summarise(pop = sum(pop), .groups = "drop")
cases <- read_csv(file.path(DIR_DATA, "cases.csv"), show_col_types = FALSE,
                  col_types = cols(age = col_character())) %>%
  apply_merge()

## analysis units: mean female population of at least MIN_POP_MODEL
units <- pop %>% group_by(cod_barrio) %>% summarise(pop = mean(pop)) %>%
  filter(pop >= MIN_POP_MODEL) %>% pull(cod_barrio)
pop     <- filter(pop, cod_barrio %in% units)
pop_age <- filter(pop_age, cod_barrio %in% units)
cases   <- filter(cases, cod_barrio %in% units)

## city-wide age-specific reference rates by type; cases with no valid age
## are pro-rated to the age distribution of their type
cases_age <- cases %>% filter(!is.na(age)) %>%
  group_by(type, age) %>% summarise(y = sum(y), .groups = "drop")
no_age <- cases %>% group_by(type) %>%
  summarise(f = sum(y) / sum(y[!is.na(age)]), .groups = "drop")
rates <- cases_age %>%
  left_join(no_age, by = "type") %>%
  left_join(pop_age %>% group_by(age) %>% summarise(pop = sum(pop)), by = "age") %>%
  mutate(rate = y * f / pop)

E <- pop_age %>%
  crossing(type = c("non_sexual", "sexual")) %>%
  left_join(select(rates, type, age, rate), by = c("type", "age")) %>%
  group_by(cod_barrio, year, type) %>%
  summarise(E = sum(pop * rate), .groups = "drop")

## final panel
y <- cases %>% group_by(cod_barrio, year, type) %>%
  summarise(y_intrafamilial = sum(y[intrafamilial]), y = sum(y), .groups = "drop")

panel <- pop %>%
  crossing(type = c("non_sexual", "sexual")) %>%
  left_join(y, by = c("cod_barrio", "year", "type")) %>%
  mutate(y = replace_na(y, 0L), y_intrafamilial = replace_na(y_intrafamilial, 0L)) %>%
  left_join(E, by = c("cod_barrio", "year", "type")) %>%
  select(cod_barrio, nombre_barrio, cod_comuna, nombre_comuna, year, type,
         pop, y, y_intrafamilial, E) %>%
  arrange(type, year, cod_barrio)
stopifnot(all(panel$E > 0), abs(sum(panel$E) / sum(panel$y) - 1) < 1e-8)

## age-specific cells, used to rebuild training-only rates in the hold-out check
panel_age <- pop_age %>%
  crossing(type = c("non_sexual", "sexual")) %>%
  left_join(cases %>% filter(!is.na(age)) %>%
              group_by(cod_barrio, year, type, age) %>% summarise(y = sum(y), .groups = "drop"),
            by = c("cod_barrio", "year", "type", "age")) %>%
  mutate(y = replace_na(y, 0L))

write_csv(panel, file.path(DIR_DATA, "panel.csv"))
write_csv(panel_age, file.path(DIR_DATA, "panel_age.csv"))
message(sprintf("Panel: %d units x %d years x 2 types; %d cases (%d sexual)",
                n_distinct(panel$cod_barrio), length(YEARS), sum(panel$y),
                sum(panel$y[panel$type == "sexual"])))
