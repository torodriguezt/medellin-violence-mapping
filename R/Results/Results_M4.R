################################################################################
# Numbers of the article from M4 (period loadings in both outcomes), written
# to results/tables/M4/ and printed: model comparison with the standard error
# of each WAIC difference, parameters, PIT and posterior predictive checks by
# outcome, hold-out coverage, dual hotspots, the spatial decomposition and its
# post hoc comparison with the IMCV, temporal factors, shared-component risk
# contrasts (discrete and continuous hyperparameter integration) and the
# agreement between M3 and M4.
# Needs the fits, posterior_draws_M4.R, holdout_M4.R and psi_continuous_M4.R.
################################################################################
suppressPackageStartupMessages(library(INLA))
OUT <- "results/tables/M4"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
FILE_IMCV <- "R/Data/IMCV/imcv_medellin.csv"
BETA <- paste("Beta for", c("idx_psi1_conf", "idx_psi1_post",
                            "idx_psi2_pre", "idx_psi2_conf", "idx_psi2_post"))
summ <- function(x) c(mean = mean(x), q025 = unname(quantile(x, 0.025)),
                      q975 = unname(quantile(x, 0.975)), P_gt_0 = mean(x > 0))
show <- function(title, x) { cat("\n==", title, "\n"); print(x, digits = 4, row.names = FALSE) }

## ---------------------------------------------------------------------------
## 1. Model comparison: WAIC, DIC and the SE of each WAIC difference,
##    from the pointwise contributions of the paired models (Vehtari 2017)
## ---------------------------------------------------------------------------
models <- c(M0 = "M0", M1 = "M1", `M2-I` = "M2_I", `M2-III` = "M2_III",
            `M3 annual` = "M3_annual", `M3 l3` = "M3_l3", M4 = "M3_l3_both_loadings_all",
            SCM = "SCM")
pw <- list(); rows <- list()
for (m in names(models)) {
  f <- readRDS(file.path("results/fits", paste0(models[[m]], ".rds")))
  pw[[m]] <- f$waic$local.waic
  rows[[m]] <- data.frame(model = m, WAIC = f$waic$waic, DIC = f$dic$dic,
                          p_WAIC = f$waic$p.eff, seconds = f$cpu.used[["Total"]])
  rm(f); invisible(gc())
}
cmp <- do.call(rbind, rows)
for (ref in c("M4", "M3 l3")) {
  d  <- sapply(names(pw), function(m) sum(pw[[m]] - pw[[ref]]))
  se <- sapply(names(pw), function(m) sqrt(length(pw[[m]])) * sd(pw[[m]] - pw[[ref]]))
  cmp[[paste0("dWAIC_vs_", sub(" ", "_", ref))]] <- d
  cmp[[paste0("SE_vs_", sub(" ", "_", ref))]] <- se
}
write.csv(cmp, file.path(OUT, "model_comparison.csv"), row.names = FALSE)
write.csv(cmp[, c("model", "seconds")], file.path(OUT, "cpu_times.csv"), row.names = FALSE)
show("Model comparison", cmp)

## ---------------------------------------------------------------------------
m4  <- readRDS("results/fits/M3_l3_both_loadings_all.rds")
m3  <- readRDS("results/fits/M3_l3.rds")
dr  <- readRDS("results/posterior/M4_draws.rds")
dat <- as.data.frame(m4$.args$data)
ns <- which(dat$resp == 1); sx <- which(dat$resp == 2)
area <- unique(dat[, c("id_area", "cod_barrio", "nombre_barrio", "cod_comuna", "nombre_comuna")])
area <- area[order(area$id_area), ]
area$women <- tapply(dat$pop[ns], dat$id_area[ns], mean)[as.character(area$id_area)]
area$urban <- as.integer(area$cod_comuna) <= 16

## ---------------------------------------------------------------------------
## 2. Parameters of M4; scales are sigma = tau^(-1/2) over the marginal of log tau
## ---------------------------------------------------------------------------
marginal_row <- function(tab, name) as.numeric(tab[name, c("mean", "0.025quant", "0.975quant")])
sigma_row <- function(term) {
  m <- m4$internal.marginals.hyperpar[[paste("Log precision for", term)]]
  c(inla.emarginal(function(x) exp(-x / 2), m), exp(-inla.qmarginal(c(0.975, 0.025), m) / 2))
}
hp <- m4$summary.hyperpar
par_rows <- list(
  alpha_1 = marginal_row(m4$summary.fixed, "resp_fnon_sexual"),
  alpha_2 = marginal_row(m4$summary.fixed, "resp_fsexual"),
  sigma_psi = sigma_row("idx_psi1_pre"), pi_psi = marginal_row(hp, "Phi for idx_psi1_pre"),
  sigma_phi1 = sigma_row("idx_phi1"), pi_phi1 = marginal_row(hp, "Phi for idx_phi1"),
  sigma_phi2 = sigma_row("idx_phi2"), pi_phi2 = marginal_row(hp, "Phi for idx_phi2"),
  delta1_res = marginal_row(hp, BETA[1]), delta1_post = marginal_row(hp, BETA[2]),
  delta2_pre = marginal_row(hp, BETA[3]), delta2_res = marginal_row(hp, BETA[4]),
  delta2_post = marginal_row(hp, BETA[5]),
  lambda = marginal_row(hp, "Beta for idx_gam_2"),
  sigma_gamma = sigma_row("idx_gam_1"), sigma_nu = sigma_row("idx_dev_2"),
  sigma_Delta1 = sigma_row("icell_1"), sigma_Delta2 = sigma_row("icell_2"))
pars <- data.frame(parameter = names(par_rows), do.call(rbind, par_rows), row.names = NULL)
names(pars)[2:4] <- c("mean", "q025", "q975")
write.csv(pars, file.path(OUT, "parameters.csv"), row.names = FALSE)
show("Parameters of M4", pars)

## ---------------------------------------------------------------------------
## 3. PIT by outcome: INLA's PIT is P(Y <= y | y_-i); the nonrandomized PIT of
##    Czado et al. (2009) spreads each cell uniformly over [P(Y <= y - 1), P(Y <= y)]
## ---------------------------------------------------------------------------
br <- seq(0, 1, 0.1)
npit <- function(u, l) sapply(seq_len(length(br) - 1), function(j)
  sum(pmax(0, pmin(br[j + 1], u) - pmax(br[j], l)) / pmax(u - l, 1e-12)))
pit_rows <- list()
for (k in 1:2) {
  i <- which(dat$resp == k)
  u <- m4$cpo$pit[i]; l <- pmax(0, u - m4$cpo$cpo[i])
  h <- npit(u, l)
  pit_rows[[k]] <- data.frame(model = "M4", outcome = c("non_sexual", "sexual")[k],
    t(setNames(round(h, 1), sprintf("bin%02d", 1:10))),
    share_below_0.2 = sum(h[1:2]) / length(i), share_above_0.8 = sum(h[9:10]) / length(i),
    row.names = NULL)
}
pit <- do.call(rbind, pit_rows)
write.csv(pit, file.path(OUT, "pit_by_outcome.csv"), row.names = FALSE)
show("Nonrandomized PIT by outcome (10 bins; 142.5 expected per bin)", pit)

## The article also reports the lower-tail mass among positive non-sexual
## counts, and midpoint PIT means by period and shared-field tertile.
positive <- ns[dat$y[ns] > 0]
u <- m4$cpo$pit[positive]; l <- pmax(0, u - m4$cpo$cpo[positive])
pit_positive <- data.frame(outcome = "non_sexual", n = length(positive),
                           share_below_0.2 = sum(npit(u, l)[1:2]) / length(positive))
write.csv(pit_positive, file.path(OUT, "pit_positive_counts.csv"), row.names = FALSE)
show("Nonrandomized PIT among positive non-sexual counts", pit_positive)

## Define tertiles once from posterior mean psi across neighbourhoods
## (type-7 quantiles, equal weight per area), then carry them to each year's cells.
psi_area <- m4$summary.random$idx_psi1_pre$mean[area$id_area]
area$psi_tertile <- cut(psi_area, breaks = quantile(psi_area, c(0, 1/3, 2/3, 1), type = 7),
                        include.lowest = TRUE, labels = c("lower", "middle", "upper"))
cell_tertile <- area$psi_tertile[match(dat$id_area[ns], area$id_area)]
midpoint <- m4$cpo$pit[ns] - m4$cpo$cpo[ns] / 2
midpoint_summary <- function(group, grouping) {
  parts <- split(midpoint, group, drop = TRUE)
  data.frame(outcome = "non_sexual", grouping = grouping, group = names(parts),
             n = lengths(parts), mean_midpoint = vapply(parts, mean, numeric(1)), row.names = NULL)
}
pit_midpoint <- rbind(
  midpoint_summary(factor(dat$period[ns], levels = c("pre", "conf", "post"),
                           labels = c("2018-2019", "2020-2021", "2022")), "period"),
  midpoint_summary(cell_tertile, "shared_field_tertile"))
write.csv(pit_midpoint, file.path(OUT, "pit_midpoint.csv"), row.names = FALSE)
show("Midpoint PIT of non-sexual counts by period and shared-field tertile", pit_midpoint)

## ---------------------------------------------------------------------------
## 4. Posterior predictive check: 500 datasets replicated from the M4 draws
## ---------------------------------------------------------------------------
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(542028)
cols <- sample.int(ncol(dr$eta), 500)
mu <- dat$E * exp(dr$eta[, cols])
set.seed(542029)
yrep <- matrix(rpois(length(mu), mu), nrow = nrow(mu))
stat <- list(total = sum, max = max, q99 = function(y) quantile(y, 0.99, type = 7),
             zeros = function(y) mean(y == 0))
ppc <- do.call(rbind, lapply(1:2, function(k) {
  i <- which(dat$resp == k)
  do.call(rbind, lapply(names(stat), function(s) {
    r <- apply(yrep[i, ], 2, stat[[s]])
    data.frame(outcome = c("non_sexual", "sexual")[k], statistic = s,
               observed = stat[[s]](dat$y[i]), replicated_mean = mean(r),
               q025 = quantile(r, 0.025), q975 = quantile(r, 0.975), row.names = NULL)
  }))
}))
zero_ns <- dat[ns, ][dat$y[ns] == 0, ]
ppc_zero <- data.frame(n_zero_ns = nrow(zero_ns), expected_zero_ns = mean(colSums(yrep[ns, ] == 0)),
                       zero_rural = sum(!area$urban[zero_ns$id_area]),
                       zero_by_year = paste(table(factor(zero_ns$year, levels = 2018:2022)), collapse = " "))
write.csv(ppc, file.path(OUT, "ppc.csv"), row.names = FALSE)
show("Posterior predictive check", ppc)
show("Zero counts of non-sexual violence", ppc_zero)

## ---------------------------------------------------------------------------
## 5. Hold-out coverage (holdout_M4.R)
## ---------------------------------------------------------------------------
ho <- read.csv(file.path(OUT, "holdout_predictions.csv"))
show("Hold-out coverage of M4",
     data.frame(type = c(sort(unique(ho$type)), "overall"),
                covered = c(tapply(ho$covered, ho$type, sum), sum(ho$covered)),
                n = c(tapply(ho$covered, ho$type, length), nrow(ho)),
                share = c(tapply(ho$covered, ho$type, mean), mean(ho$covered))))

## ---------------------------------------------------------------------------
## 6. Dual hotspots from 1,000 of the 2,000 draws, as Table_hotspots.R does for M3
## ---------------------------------------------------------------------------
set.seed(532027)
hcols <- sort(sample.int(ncol(dr$eta), 1000))
exc_ns <- dr$eta[ns, hcols] > 0
exc_sx <- dr$eta[sx, hcols] > 0
cells <- data.frame(dat[ns, c("id_area", "cod_barrio", "nombre_barrio", "nombre_comuna", "year")],
                    RR_ns = exp(m4$summary.linear.predictor$mean[ns]),
                    RR_sx = exp(m4$summary.linear.predictor$mean[sx]),
                    P_ns = rowMeans(exc_ns), P_sx = rowMeans(exc_sx),
                    P_joint = rowMeans(exc_ns & exc_sx), row.names = NULL)
write.csv(cells, file.path(OUT, "joint_exceedance.csv"), row.names = FALSE)

## share of each unit's notifications assigned by name matching
cases <- read.csv("results/data/cases.csv", colClasses = c(cod_barrio = "character"))
merge_map <- read.csv("results/data/unit_merge.csv", colClasses = "character")
cases$unit <- merge_map$cod_destino[match(cases$cod_barrio, merge_map$cod_barrio)]
cases$unit[is.na(cases$unit)] <- cases$cod_barrio[is.na(cases$unit)]
name_share <- tapply(cases$y * cases$name_assigned, cases$unit, sum) / tapply(cases$y, cases$unit, sum)

hotspot_rules <- function(cl) {
  by <- split(cl, cl$cod_barrio)
  do.call(rbind, lapply(by, function(d) data.frame(
    cod_barrio = d$cod_barrio[1], p95_all_years = all(d$P_joint > 0.95),
    min_P_joint = min(d$P_joint))))
}
rules_m4 <- hotspot_rules(cells)
cells_m3 <- read.csv("results/tables/joint_exceedance.csv", colClasses = c(cod_barrio = "character"))
cells_cod <- read.csv(file.path(OUT, "joint_exceedance_coded_only.csv"),
                      colClasses = c(cod_barrio = "character"))   # geocoding_coded_only_M4.R
rules_cod <- hotspot_rules(cells_cod)
hot <- merge(rules_m4, area[, c("cod_barrio", "nombre_barrio", "nombre_comuna", "women")], by = "cod_barrio")
hot$RR_ns <- tapply(cells$RR_ns, cells$cod_barrio, mean)[hot$cod_barrio]
hot$RR_sx <- tapply(cells$RR_sx, cells$cod_barrio, mean)[hot$cod_barrio]
hot$name_share <- name_share[hot$cod_barrio]
hot$coded_p95 <- rules_cod$p95_all_years[match(hot$cod_barrio, rules_cod$cod_barrio)]
hot <- hot[hot$p95_all_years, ]
hot <- hot[order(hot$cod_barrio), c("cod_barrio", "nombre_barrio", "nombre_comuna",
                                  "women", "RR_ns", "RR_sx", "name_share",
                                  "p95_all_years", "coded_p95", "min_P_joint")]
write.csv(hot, file.path(OUT, "hotspots.csv"), row.names = FALSE)
show("Persistent dual hotspots (P_joint > 0.95 in every year)", hot)
show("Persistent hotspot summary",
     data.frame(neighbourhoods = nrow(hot), retained_coded_only = sum(hot$coded_p95),
                women_under_500 = sum(hot$women < 500), median_women_all_units = median(area$women),
                lowest_included_probability = min(hot$min_P_joint),
                highest_excluded_probability = max(rules_m4$min_P_joint[!rules_m4$p95_all_years])))
show("Units with P_joint > 0.8 by year (M4)", tapply(cells$P_joint > 0.8, cells$year, sum))
u <- subset(cells, P_ns > 0.05 & P_ns < 0.95 & P_sx > 0.05 & P_sx < 0.95)
jr <- u$P_joint / (u$P_ns * u$P_sx)
show("Joint / product of marginals (both marginals in 0.05-0.95)",
     data.frame(n = nrow(u), mean = mean(jr), max = max(jr), below_one = sum(jr < 1)))

## ---------------------------------------------------------------------------
## 7. Spatial decomposition: psi (units of non-sexual log-risk in 2018-2019)
## ---------------------------------------------------------------------------
psi <- m4$summary.random$idx_psi1_pre$mean[1:285]
phi1 <- m4$summary.random$idx_phi1$mean[1:285]
by_commune <- aggregate(data.frame(psi = psi), list(commune = area$nombre_comuna), mean)
by_commune$factor <- exp(by_commune$psi)
by_commune <- by_commune[order(-by_commune$psi), ]
write.csv(by_commune, file.path(OUT, "psi_by_commune.csv"), row.names = FALSE)
show("exp(mean psi) by commune", by_commune)
show("phi_1: mean outside communes 1-16, inside, and Pearson cor with psi",
     data.frame(rural = mean(phi1[!area$urban]), urban = mean(phi1[area$urban]),
                cor_phi1_psi = cor(phi1, psi), n_rural = sum(!area$urban)))

## post hoc rank correlation of commune averages of psi with the 2018 IMCV,
## within each of the 2,000 joint draws
imcv <- read.csv(FILE_IMCV, colClasses = c(varcharidcomunafk = "character"), check.names = FALSE)
imcv <- imcv[imcv[[2]] == 2018 & as.integer(imcv$varcharidcomunafk) <= 16, ]
cm <- sprintf("%02d", as.integer(area$cod_comuna))
urb <- which(area$urban)
rank_cor <- apply(dr$psi, 2, function(p) {
  avg <- tapply(p[urb], cm[urb], mean)
  cor(avg, imcv$decimcv[match(names(avg), sprintf("%02d", as.integer(imcv$varcharidcomunafk)))],
      method = "spearman")
})
show("Rank correlation of commune psi with IMCV 2018 (16 urban communes)", t(summ(rank_cor)))

## ---------------------------------------------------------------------------
## 8. Temporal factors exp(alpha_1 + gamma_t) and exp(alpha_2 + lambda gamma_t + nu_t)
## ---------------------------------------------------------------------------
x <- dr$other
tf_ns <- t(sapply(1:5, function(k) exp(x["resp_fnon_sexual:1", ] + x[paste0("idx_gam_1:", k), ])))
tf_sx <- t(sapply(1:5, function(k) exp(x["resp_fsexual:1", ] + x[paste0("idx_gam_2:", k), ] +
                                          x[paste0("idx_dev_2:", k), ])))
tf <- rbind(data.frame(type = "non_sexual", year = 2018:2022, t(apply(tf_ns, 1, summ))),
            data.frame(type = "sexual", year = 2018:2022, t(apply(tf_sx, 1, summ))))
write.csv(tf, file.path(OUT, "temporal_factors.csv"), row.names = FALSE)
show("Temporal factors", tf[, 1:5])

## ---------------------------------------------------------------------------
## 9. Shared-component risk contrasts between the 90th and 10th percentiles of
##    psi, within paired draws of the field and the loadings
## ---------------------------------------------------------------------------
risk_contrasts <- function(psi_draws, L) {
  g <- apply(psi_draws, 2, function(p) diff(quantile(p, c(0.1, 0.9), type = 7)))
  q <- list(g = g,
            ns_pre = exp(g), ns_res = exp(L[, 1] * g), ns_post = exp(L[, 2] * g),
            sx_pre = exp(L[, 3] * g), sx_res = exp(L[, 4] * g), sx_post = exp(L[, 5] * g),
            ns_res_over_pre = exp((L[, 1] - 1) * g), sx_res_over_pre = exp((L[, 4] - L[, 3]) * g),
            ns_post_over_pre = exp((L[, 2] - 1) * g), sx_post_over_pre = exp((L[, 5] - L[, 3]) * g))
  data.frame(quantity = names(q), t(sapply(q, summ)), row.names = NULL)
}
read_continuous_draws <- function(path, n_area, hyperparameter_names) {
  fail <- function(reason) stop(
    "The article requires all 5,000 paired continuous M4 draws: ", reason,
    ". Complete or repair results/posterior/M4_psi_continuous with psi_continuous_M4.R.",
    call. = FALSE)
  cont_files <- list.files(path, "^draws_.*\\.rds$", full.names = TRUE)
  hyper_file <- file.path(path, "hyperparameters.rds")
  if (!length(cont_files) || !file.exists(hyper_file)) fail("the draw bank is missing")
  parts <- lapply(cont_files, readRDS)
  valid <- vapply(parts, function(p) {
    is.list(p) && is.numeric(p$ids) && length(p$ids) > 0L &&
      all(is.finite(p$ids)) && all(p$ids == trunc(p$ids)) &&
      is.matrix(p$psi) && is.numeric(p$psi) && nrow(p$psi) == n_area &&
      ncol(p$psi) == length(p$ids) && all(is.finite(p$psi))
  }, logical(1))
  if (!all(valid)) fail("a batch has invalid draw IDs or shared-field dimensions/values")
  ids <- unlist(lapply(parts, `[[`, "ids"))
  if (length(ids) != 5000L || anyDuplicated(ids) ||
      !identical(as.integer(sort(ids)), seq_len(5000L)))
    fail("draw IDs must contain 1 through 5000 exactly once")
  H <- readRDS(hyper_file)
  if (!is.matrix(H) || !is.numeric(H) || nrow(H) != 5000L ||
      !identical(colnames(H), hyperparameter_names) || !all(is.finite(H)))
    fail("the hyperparameter bank must have 5,000 rows and match the M4 hyperparameters")
  list(psi = do.call(cbind, lapply(parts, `[[`, "psi"))[, order(ids), drop = FALSE],
       hyper = H)
}
bank <- read_continuous_draws("results/posterior/M4_psi_continuous", nrow(area),
                              rownames(m4$summary.hyperpar))
discrete <- risk_contrasts(dr$psi, dr$hyper[, BETA, drop = FALSE])
continuous <- risk_contrasts(bank$psi, bank$hyper[, BETA, drop = FALSE])
rc <- merge(discrete, continuous, by = "quantity", suffixes = c("_discrete", "_continuous"),
            sort = FALSE)
rc$width_ratio <- (rc$q975_continuous - rc$q025_continuous) / (rc$q975_discrete - rc$q025_discrete)
rc$n_continuous <- ncol(bank$psi)
write.csv(rc, file.path(OUT, "risk_contrasts.csv"), row.names = FALSE)
show("Shared-component risk contrasts (P90 vs P10 of psi)", rc)

## ---------------------------------------------------------------------------
## 10. M3 for the sentences that keep it: correlation of the posterior mean
##     non-sexual interaction with psi by year, and agreement with M4
## ---------------------------------------------------------------------------
psi3 <- m3$summary.random$idx_psi_1$mean[1:285]
d1 <- matrix(m3$summary.random$icell_1$mean, nrow = 285)   # cell = (year - 1) * 285 + area
show("M3: cor(posterior mean Delta_i1t, psi_i) by year",
     setNames(round(apply(d1, 2, cor, psi3), 3), 2018:2022))
e3 <- m3$summary.linear.predictor$mean; e4 <- m4$summary.linear.predictor$mean
show("Agreement of M3 and M4",
     data.frame(cor_logRR_ns = cor(e3[ns], e4[ns]), cor_logRR_sx = cor(e3[sx], e4[sx]),
                cor_Pjoint = cor(cells_m3$P_joint[match(paste(cells$cod_barrio, cells$year),
                                                        paste(cells_m3$cod_barrio, cells_m3$year))],
                                 cells$P_joint),
                cor_psi = cor(psi3, psi), seconds_M4 = m4$cpu.used[["Total"]]))
