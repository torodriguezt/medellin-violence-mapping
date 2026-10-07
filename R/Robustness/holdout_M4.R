################################################################################
# Hold-out validation of M4 reported in the article: a random 15% of the
# 2,850 neighbourhood-year-outcome cells is withheld, the age-specific reference
# rates and expected counts are rebuilt from the training cells only, M4 is
# refitted, and 1,000 posterior predictive counts are drawn for every withheld
# cell. The loadings start at 0, and at 1 if that reaches negative loadings.
################################################################################
source("R/All_Models/model_data.R")
source("R/All_Models/model_M4_setup.R")
dir.create("results/tables/M4", recursive = TRUE, showWarnings = FALSE)

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

fit <- fit_m4(m4, dat, full = TRUE)
if (any(fit$summary.hyperpar[BETA, "mean"] < 0)) {
  LOAD_INIT <- rep(1, 5)
  fit <- fit_m4(m4, dat, full = TRUE)
}
message(sprintf("M4 hold-out fit: mlik %.2f, loadings %s", fit$mlik[1, 1],
                paste(sprintf("%.3f", fit$summary.hyperpar[BETA, "mean"]), collapse = " ")))

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
write.csv(pred, "results/tables/M4/holdout_predictions.csv", row.names = FALSE)

coverage <- aggregate(covered ~ type, data = pred, FUN = mean)
coverage <- rbind(coverage, data.frame(type = "overall", covered = mean(pred$covered)))
write.csv(coverage, "results/tables/M4/holdout_coverage.csv", row.names = FALSE)
print(coverage)
