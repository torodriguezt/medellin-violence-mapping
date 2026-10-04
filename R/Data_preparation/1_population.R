################################################################################
# Female population by neighbourhood, year and five-year age group, 2018-2022,
# from the official small-area projections of Medellin (2018 census base).
################################################################################
source("R/Data_preparation/config.R")

read_sheet <- function(sheet, by_age) {
  fixed <- c("cod_comuna", "nombre_comuna", "cod_barrio", "nombre_barrio",
             "area", "sex", if (by_age) "age_range")
  raw <- read_excel(FILE_POPULATION, sheet = sheet, skip = 14, col_names = FALSE,
                    .name_repair = "unique_quiet")
  raw <- raw[, seq_len(length(fixed) + 13)]
  names(raw) <- c(fixed, 2018:2030)
  raw %>%
    filter(!is.na(cod_barrio), !is.na(sex), str_trim(area) == "Total") %>%
    mutate(cod_barrio = normalise_code(cod_barrio)) %>%
    pivot_longer(as.character(2018:2030), names_to = "year", values_to = "pop") %>%
    mutate(year = as.integer(year), pop = suppressWarnings(as.numeric(pop))) %>%
    filter(!is.na(pop), sex == "Mujeres", year %in% YEARS)
}

sheets <- excel_sheets(FILE_POPULATION)
sheet_total <- sheets[str_detect(sheets, "Sexo") & !str_detect(sheets, "Quin")]
sheet_age   <- sheets[str_detect(sheets, "Sexo") &  str_detect(sheets, "Quin")]
stopifnot(length(sheet_total) == 1, length(sheet_age) == 1)

## neighbourhood x year
pop <- read_sheet(sheet_total, by_age = FALSE) %>%
  select(cod_comuna, nombre_comuna, cod_barrio, nombre_barrio, year, pop)

## neighbourhood x year x age group (85+ joined to 80-84)
pop_age <- read_sheet(sheet_age, by_age = TRUE) %>%
  mutate(age = ifelse(str_trim(age_range) %in% c("80-84", "85 y más", "85 y mas"),
                      "80+", str_trim(age_range))) %>%
  group_by(cod_barrio, year, age) %>%
  summarise(pop = sum(pop), .groups = "drop")
stopifnot(all(pop_age$age %in% AGE_GROUPS))

total_2020 <- sum(pop$pop[pop$year == 2020])
stopifnot(total_2020 > 1.2e6, total_2020 < 1.45e6,
          abs(sum(pop_age$pop[pop_age$year == 2020]) / total_2020 - 1) < 0.02)

write_csv(pop, file.path(DIR_DATA, "population.csv"))
write_csv(pop_age, file.path(DIR_DATA, "population_age.csv"))
message(sprintf("Population: %d neighbourhoods, %s women in 2020",
                n_distinct(pop$cod_barrio), format(round(total_2020), big.mark = ",")))
