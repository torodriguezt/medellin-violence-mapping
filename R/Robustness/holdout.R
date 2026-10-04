################################################################################
# Hold-out validation of M3 (l = 3). A random 15% of the 2,850 neighbourhood-
# year-outcome cells is withheld; the age-specific reference rates and expected
# counts are rebuilt from the training cells only, the model is refitted, and
# 1,000 posterior predictive counts are drawn for every withheld cell.
################################################################################
source("R/All_Models/model_data.R")
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026)
ho <- sort(sample(nrow(dat), round(0.15 * nrow(dat))))   # 428 cells

## expected counts from training-only age-specific rates
key <- function(d) paste(d$cod_barrio, d$year, d$type)
cells <- read.csv("results/data/panel_age.csv", colClasses = c(cod_barrio = "character"))
train <- cells[!key(cells) %in% key(dat[ho, ]), ]
rates <- aggregate(cbind(y, pop) ~ type + age, data = train, FUN = sum)
cells$rate <- with(rates, y / pop)[match(paste(cells$type, cells$age),
                                         paste(rates$type, rates$age))]
E_train <- tapply(cells$pop * cells$rate, key(cells), sum)

dat$y_obs <- dat$y
dat$E     <- as.numeric(E_train[key(dat)])
dat$y[ho] <- NA

formula <- y ~ 0 + resp_f +
  f(idx_psi_1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi2_pre,  copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_conf, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_psi2_post, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0) +
  f(icell_2, model = "iid", hyper = PC_PREC,
    constr = FALSE, extraconstr = C_I, rankdef = 2L, diagonal = 0)

fit <- fit_model(formula, "M3_l3_holdout")

## posterior predictive counts: joint draws of log RR, then Poisson noise
set.seed(562026)
draws <- inla.posterior.sample(1000, fit, selection = list(Predictor = ho),
                               seed = 562027, num.threads = "1:1", parallel.configs = FALSE,
                               use.improved.mean = TRUE, skew.corr = FALSE)
eta <- sapply(draws, function(s) s$latent[paste0("Predictor:", ho), 1])
mu  <- dat$E[ho] * exp(eta)
set.seed(562028)
yrep <- matrix(rpois(length(mu), mu), nrow = length(ho))

pred <- data.frame(dat[ho, c("cod_barrio", "year", "type")], observed = dat$y_obs[ho],
                   predicted = rowMeans(mu),
                   lower = apply(yrep, 1, quantile, 0.025, type = 1),
                   upper = apply(yrep, 1, quantile, 0.975, type = 1))
pred$covered <- pred$observed >= pred$lower & pred$observed <= pred$upper
write.csv(pred, "results/tables/holdout_predictions.csv", row.names = FALSE)

coverage <- aggregate(covered ~ type, data = pred, FUN = mean)
coverage <- rbind(coverage, data.frame(type = "overall", covered = mean(pred$covered)))
write.csv(coverage, "results/tables/holdout_coverage.csv", row.names = FALSE)
print(coverage)
