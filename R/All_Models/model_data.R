# Data, priors, constraints and fitting function shared by every model.
# Sourced at the top of each model script; run from the repository root.

suppressPackageStartupMessages({library(INLA); library(Matrix)})
inla.setOption(num.threads = "2:1")

dir.create("results/fits", recursive = TRUE, showWarnings = FALSE)

## readr parses the expected counts exactly as they were written (base read.csv
## can be off in the last bit, enough to move INLA's optimiser slightly)
g     <- inla.read.graph("results/data/barrios_medellin.graph")
areas <- as.data.frame(readr::read_csv("results/data/areas.csv", show_col_types = FALSE))
panel <- as.data.frame(readr::read_csv("results/data/panel.csv", show_col_types = FALSE,
                                       col_types = readr::cols(cod_comuna = "c")))

n_area <- g$n
n_year <- 5

## one row per neighbourhood-year-outcome, ordered outcome > year > area
dat <- merge(panel, areas[, c("id_area", "cod_barrio")], by = "cod_barrio")
dat$resp    <- ifelse(dat$type == "non_sexual", 1L, 2L)
dat$resp_f  <- factor(dat$resp, levels = 1:2, labels = c("non_sexual", "sexual"))
dat$id_year <- dat$year - 2017L
dat$period  <- c("pre", "pre", "conf", "conf", "post")[dat$id_year]
dat <- dat[order(dat$resp, dat$id_year, dat$id_area), ]
rownames(dat) <- NULL
stopifnot(nrow(dat) == 2 * n_area * n_year)

## outcome-specific indices: NA on the rows of the other outcome
only <- function(k, x) ifelse(dat$resp == k, x, NA)
cell <- (dat$id_year - 1L) * n_area + dat$id_area
dat$idx_psi_1 <- only(1, dat$id_area)    # shared spatial field psi
dat$idx_psi_2 <- only(2, dat$id_area)    # psi copied into the sexual outcome
dat$idx_phi1  <- only(1, dat$id_area)    # outcome-specific spatial fields
dat$idx_phi2  <- only(2, dat$id_area)
dat$idx_gam_1 <- only(1, dat$id_year)    # shared temporal trend gamma
dat$idx_gam_2 <- only(2, dat$id_year)    # gamma copied into the sexual outcome
dat$idx_dev_2 <- only(2, dat$id_year)    # sexual-specific temporal deviation nu
dat$ia_1 <- only(1, dat$id_area); dat$ia_2 <- only(2, dat$id_area)
dat$iy_1 <- only(1, dat$id_year); dat$iy_2 <- only(2, dat$id_year)
dat$icell_1 <- only(1, cell); dat$icell_2 <- only(2, cell)   # space-time cells
## psi copied once per period (M3, l = 3) and once per year (M3, l = T)
for (p in c("pre", "conf", "post"))
  dat[[paste0("idx_psi2_", p)]] <- ifelse(dat$resp == 2 & dat$period == p, dat$id_area, NA)
for (t in 1:n_year)
  dat[[paste0("idx_psi2_t", t)]] <- ifelse(dat$resp == 2 & dat$id_year == t, dat$id_area, NA)

## shared-interaction model (SCM), in the layout of Retegui et al. (2024)
dat$alpha1   <- as.numeric(dat$resp == 1)
dat$alpha2   <- as.numeric(dat$resp == 2)
dat$ID_area  <- dat$id_area + n_area * (dat$resp - 1)   # besag2: outcome 1 | outcome 2
dat$ID_unst  <- only(2, dat$id_area)                    # heterogeneity, sexual only
dat$ID_time1 <- only(1, dat$id_year)
dat$ID_time2 <- only(2, dat$id_year)
dat$ID_area1 <- cell + n_area * n_year * (dat$resp - 1) # shared interaction, 1..2AT

## priors
PC_BYM2 <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)),   # P(sigma > 1) = 0.01
                phi  = list(prior = "pc",      param = c(0.5, 2/3)))  # P(pi < 0.5) = 2/3
PC_PREC <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
PC_BETA <- list(beta = list(prior = "normal", param = c(0, 1), fixed = FALSE))   # loadings

## centred Type I interaction: zero sum and zero linear-time projection per outcome
A_I <- rbind(rep(1, n_area * n_year), rep(1:n_year - mean(1:n_year), each = n_area))
C_I <- list(A = A_I / sqrt(rowSums(A_I^2)), e = c(0, 0))

## E = E is evaluated inside `data`: the expected-count column.
## restart = FALSE and two threads are the settings of the published fits.
fit_model <- function(formula, name, data = dat) {
  message("Fitting ", name, " ...")
  fit <- inla(formula, family = "poisson", data = data, E = E,
              control.predictor = list(compute = TRUE),
              control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE,
                                     return.marginals.predictor = TRUE),
              control.mode = list(restart = FALSE))
  saveRDS(fit, file.path("results/fits", paste0(name, ".rds")))
  message(sprintf("  %s: WAIC %.1f, DIC %.1f, %.0f s", name, fit$waic$waic,
                  fit$dic$dic, fit$cpu.used[["Total"]]))
  invisible(fit)
}
