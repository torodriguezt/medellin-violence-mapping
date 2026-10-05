################################################################################
# Cases of violence against women by neighbourhood, year, type, age group and
# perpetrator relationship. This is the only script that reads the microdata.
################################################################################
source("R/Data_preparation/config.R")

medata <- read_csv(FILE_CASES, show_col_types = FALSE,
                   locale = locale(encoding = "UTF-8"))

## notifying unit: nom_upgd is only in the raw MEData export, so each cleaned
## record is linked to it on the notification date, age, type of violence and
## fields the cleaning left unchanged, in passes with fewer fields; a key links
## only when it is unique among the remaining records of both files
link_notifier <- function(medata) {
  if (!file.exists(FILE_CASES_RAW)) return(rep(NA_character_, nrow(medata)))
  raw <- read_csv(FILE_CASES_RAW, col_types = cols(.default = col_character()),
                  locale = locale(encoding = "UTF-8"), progress = FALSE)
  norm <- function(x) {
    x <- sub("[.]0+$", "", toupper(str_trim(as.character(x))))
    ifelse(is.na(x) | x == "NA", "", x)
  }
  fixed <- c("sexo_", "tip_ss_", "per_etn_", "sexo_agre", "conv_agre", "con_fin_", "pac_hos_",
             "escenario", "ambito_lug", "zona_conf", "remit_prot", "inf_aut", "ac_mental",
             "orient_sex", "ident_gene", "consum_spa", "mujer_cabf", "antec", "sust_vict",
             "sem_ges_", "cod_dpto_r", "cod_dpto_o", "cod_pais_o", "gp_otros")
  d <- medata %>%
    transmute(row = row_number(), date = as.character(as.Date(fec_not)),
              age = norm(edad), nat = norm(def_naturaleza), code = norm(codigo_barrio),
              commune = norm(codigo_comuna), across(all_of(fixed), norm)) %>%
    filter(sexo_ == "F")
  r <- raw %>%
    transmute(rid = row_number(),
              notifier = ifelse(str_detect(toupper(coalesce(nom_upgd, "")), "COMISAR"),
                                "commissary", "health"),
              date = as.character(as.Date(fec_not, "%d/%m/%Y")),
              age = ifelse(uni_med_ == "1", norm(edad_), ""),
              nat = norm(coalesce(nat_viosex, naturaleza)), code = norm(codigo_barrio),
              commune = norm(codigo_comuna), across(all_of(fixed), norm)) %>%
    filter(sexo_ == "F", date %in% d$date)
  passes <- list(c("date", "age", "nat", "code", "commune", fixed),
                 c("date", "age", "nat", fixed),
                 c("date", "nat", "code", fixed),
                 c("date", "age", "nat", "code", "sexo_"))
  links <- tibble(row = integer(), rid = integer(), notifier = character())
  for (k in passes) {
    unique_keys <- function(x) x %>% add_count(across(all_of(k)), name = "n_key") %>%
      filter(n_key == 1) %>% select(-n_key)
    m <- inner_join(unique_keys(filter(d, !row %in% links$row))[c("row", k)],
                    unique_keys(filter(r, !rid %in% links$rid))[c("rid", "notifier", k)],
                    by = k)
    links <- bind_rows(links, select(m, row, rid, notifier))
  }
  out <- rep(NA_character_, nrow(medata))
  out[links$row] <- links$notifier
  out
}
medata$notifier <- link_notifier(medata)

cases <- medata %>%
  mutate(
    year = as.integer(.data[["año_not"]]),
    # keep the original residence code when valid; cod_barrio_nuevo was
    # assigned by name and misplaces cases whose name repeats across communes
    code_orig = normalise_code(codigo_barrio),
    code_new  = normalise_code(cod_barrio_nuevo),
    orig_ok   = coalesce(str_detect(code_orig, "^[0-9]{4}$") & code_orig != "0000", FALSE),
    cod_barrio = ifelse(orig_ok, code_orig, code_new),
    name_assigned = !orig_ok,
    cod_barrio = ifelse(cod_barrio %in% names(REMAP_CODES),
                        REMAP_CODES[cod_barrio], cod_barrio),
    type = ifelse(as.character(def_naturaleza) %in% SEXUAL_CODES, "sexual", "non_sexual"),
    neglect = coalesce(as.character(def_naturaleza) == "3", FALSE),   # neglect and abandonment
    age  = age_group(edad),
    intrafamilial = str_detect(coalesce(parentezco_agresor, ""), INTRAFAMILIAL),
    notifier = coalesce(notifier, "unlinked")
  ) %>%
  filter(year %in% YEARS, sexo_ == "F", !is.na(cod_barrio), cod_barrio != "00NA")

counts <- cases %>%
  count(cod_barrio, year, type, age, intrafamilial, name_assigned, neglect, notifier,
        name = "y")
print(with(cases, round(100 * prop.table(table(year, notifier), 1), 1)))   # % by notifier

write_csv(counts, file.path(DIR_DATA, "cases.csv"))
message(sprintf("Cases: %d women, %d neighbourhood codes, %.1f%% intrafamilial, %.1f%% assigned by name",
                nrow(cases), n_distinct(cases$cod_barrio), 100 * mean(cases$intrafamilial),
                100 * mean(cases$name_assigned)))
