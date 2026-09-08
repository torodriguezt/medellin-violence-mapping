# run full pipeline from repo root: source("main.R")

pasos <- c(
  "01_population.R",
  "02_cases_covariates.R",
  "02b_exogenous_covariates.R",
  "03_expected_counts.R",
  "04_neighbourhood_graph.R",
  "05_baseline_models.R",
  "06_dynamic_delta_model.R",
  "07_covariate_models.R",
  "08_joint_exceedance.R",
  "09_figures.R"
)

for (p in pasos) {
  message(">>> ", p)
  source(file.path("scripts", p))
}

message("done -> results/, figures/")
