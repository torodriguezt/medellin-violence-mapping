# M4: period-specific loadings of the shared field psi in both outcomes, with
# the non-sexual loading of 2018-2019 fixed at one to set the scale of psi.
# Sourced after model_data.R by the M4 scripts. The loading prior (mean,
# precision) and the starting values are read from LOAD_PRIOR and LOAD_INIT
# when inla() evaluates the formula, so they can be changed between fits.
# Terms preserve the order of the original M4 fit, so the hyperparameters
# of every fit line up with those of the saved M4 fit.

for (p in c("pre", "conf", "post"))
  dat[[paste0("idx_psi1_", p)]] <- ifelse(dat$resp == 1 & dat$period == p, dat$id_area, NA)

M4_FIT <- "results/fits/M3_l3_both_loadings_all.rds"   # historical filename, model_M4.R

## the five free loadings, in the order of LOAD_INIT
LOADINGS <- c("idx_psi1_conf", "idx_psi1_post", "idx_psi2_pre", "idx_psi2_conf", "idx_psi2_post")
BETA <- paste("Beta for", LOADINGS)

LOAD_PRIOR <- c(0, 1)
LOAD_INIT  <- rep(0, 5)
load_hyper <- function(j) list(beta = list(prior = "normal", param = LOAD_PRIOR,
                                           fixed = FALSE, initial = LOAD_INIT[j]))

## main specification: BYM2 fields and the centred Type I interaction
m4 <- y ~ 0 + resp_f +
  f(idx_psi1_pre, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi1_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(1)) +
  f(idx_psi1_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(2)) +
  f(idx_psi2_pre,  copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(3)) +
  f(idx_psi2_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(4)) +
  f(idx_psi2_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(5)) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

## spatial-prior sensitivity: scaled intrinsic CAR fields instead of BYM2
m4_icar <- y ~ 0 + resp_f +
  f(idx_psi1_pre, model = "besag", graph = g, scale.model = TRUE, hyper = PC_PREC) +
  f(idx_psi1_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(1)) +
  f(idx_psi1_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(2)) +
  f(idx_psi2_pre,  copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(3)) +
  f(idx_psi2_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(4)) +
  f(idx_psi2_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(5)) +
  f(idx_phi1, model = "besag", graph = g, scale.model = TRUE, hyper = PC_PREC) +
  f(idx_phi2, model = "besag", graph = g, scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

## interaction sensitivity: the Type III interaction of M2-III instead of Type I
m4_typeIII <- y ~ 0 + resp_f +
  f(idx_psi1_pre, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi1_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(1)) +
  f(idx_psi1_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(2)) +
  f(idx_psi2_pre,  copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(3)) +
  f(idx_psi2_conf, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(4)) +
  f(idx_psi2_post, copy = "idx_psi1_pre", fixed = FALSE, hyper = load_hyper(5)) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(ia_1, model = "besag", graph = g, scale.model = TRUE, constr = TRUE,
    rankdef = 1L, diagonal = 0, replicate = iy_1, nrep = 5L, hyper = PC_PREC) +
  f(ia_2, model = "besag", graph = g, scale.model = TRUE, constr = TRUE,
    rankdef = 1L, diagonal = 0, replicate = iy_2, nrep = 5L, hyper = PC_PREC)

## outcome and expected counts of each case definition; the relationship strata
## use crude outcome-specific rates, as in the stratified analysis
crude <- function(d) d$pop * ave(d$y, d$resp, FUN = sum) / ave(d$pop, d$resp, FUN = sum)
m4_data <- function(variant) {
  d <- dat
  switch(variant,
    all           = d,
    adults        = { d$y <- d$y_adult;     d$E <- d$E_adult;     d },
    no_neglect    = { d$y <- d$y_noneglect; d$E <- d$E_noneglect; d },
    health        = { d$y <- d$y_health;    d$E <- d$E_health;    d },
    health_intrafamilial = { d$y <- d$y_health_intrafamilial; d$E <- crude(d); d },
    health_complement = { d$y <- d$y_health - d$y_health_intrafamilial; d$E <- crude(d); d },
    coded_only    = { d$y <- d$y_coded;     d$E <- d$E_coded;     d },
    intrafamilial = { d$y <- d$y_intrafamilial; d$E <- crude(d); d },
    complement    = { d$y <- d$y - d$y_intrafamilial; d$E <- crude(d); d },
    stop("unknown variant ", variant))
}

## light fit for comparisons across starting values; full = TRUE keeps what
## posterior sampling, CPO and the predictor marginals need
fit_m4 <- function(formula, data, restart = FALSE, full = FALSE) {
  inla(formula, family = "poisson", data = data, E = E,
       control.predictor = list(compute = TRUE),
       control.compute = list(config = full, dic = TRUE, waic = TRUE, cpo = full,
                              return.marginals.predictor = full),
       control.mode = list(restart = restart))
}

## ratios and period contrasts of the loadings within joint hyperparameter draws
m4_contrasts <- function(hs) {
  ns <- cbind(1, hs[, BETA[1]], hs[, BETA[2]])
  sx <- hs[, BETA[3:5]]
  r  <- sx / ns
  cbind(ns_res_pre = ns[, 2] - 1, sx_res_pre = sx[, 2] - sx[, 1], ratio_res_pre = r[, 2] - r[, 1],
        ns_post_res = ns[, 3] - ns[, 2], sx_post_res = sx[, 3] - sx[, 2],
        ratio_post_res = r[, 3] - r[, 2],
        ns_post_pre = ns[, 3] - 1, sx_post_pre = sx[, 3] - sx[, 1], ratio_post_pre = r[, 3] - r[, 1],
        ratio_pre = r[, 1], ratio_res = r[, 2], ratio_post = r[, 3])
}
