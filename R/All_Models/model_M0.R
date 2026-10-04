################################################################################
# M0: separable baseline. Each outcome has its own BYM2 spatial field and RW2
# trend, linked only by exchangeable between-outcome correlations.
################################################################################
source("R/All_Models/model_data.R")

formula <- y ~ 0 + resp_f +
  f(id_area, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2,
    group = resp, control.group = list(model = "exchangeable")) +
  f(id_year, model = "rw2", scale.model = TRUE, hyper = PC_PREC,
    group = resp, control.group = list(model = "exchangeable"))

fit_model(formula, "M0")
