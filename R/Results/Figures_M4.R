################################################################################
# Model-based figures of the article from M4, in figures/M4/ (PNG and PDF):
# PIT by outcome, posterior predictive check, hold-out validation, temporal
# factors, relative-risk maps, fitted risk in four neighbourhoods, spatial
# components and dual hotspots. Fig 8 (loadings by period) is already M4 and
# stays in Figures.R. Needs posterior_draws_M4.R, holdout_M4.R and Results_M4.R.
################################################################################
suppressPackageStartupMessages({library(INLA); library(sf); library(dplyr); library(ggplot2)})
source("R/Results/figure_style.R")
DIR <- "figures/M4"
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)

carto <- readRDS("results/data/carto.rds")
fit   <- readRDS("results/fits/M3_l3_both_loadings_all.rds")
dat   <- as.data.frame(fit$.args$data)
dat$t <- ifelse(dat$type == "non_sexual", "Non-sexual", "Sexual")
draws <- readRDS("results/posterior/M4_draws.rds")

SELECTED <- c("0717" = "Robledo", "0607" = "Kennedy", "1412" = "San Lucas",
              "AUC2" = "San Antonio de Prado")

## PIT by outcome: nonrandomized PIT histogram (Czado et al. 2009), 10 bins
br <- seq(0, 1, 0.1)
pit <- do.call(rbind, lapply(c("Non-sexual", "Sexual"), function(k) {
  i <- which(dat$t == k)
  u <- fit$cpo$pit[i]; l <- pmax(0, u - fit$cpo$cpo[i])
  data.frame(outcome = k, lo = head(br, -1), hi = br[-1],
             mass = sapply(seq_len(length(br) - 1), function(j)
               sum(pmax(0, pmin(br[j + 1], u) - pmax(br[j], l)) / pmax(u - l, 1e-12))),
             expected = length(i) / 10)
}))
p <- ggplot(pit) +
  geom_rect(aes(xmin = lo, xmax = hi, ymin = 0, ymax = mass), fill = "grey83",
            color = "grey35", linewidth = 0.3) +
  geom_hline(aes(yintercept = expected), linetype = "dashed", color = "grey45") +
  facet_wrap(~outcome, ncol = 1) +
  labs(x = "PIT value", y = "Count") + theme_paper(legend = "none")
save_fig("fig_pit.png", p, 4.2, 4.6, DIR)

## posterior predictive check, 500 replicated datasets
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(542028)
cols <- sample.int(ncol(draws$eta), 500)
mu <- dat$E * exp(draws$eta[, cols])
set.seed(542029)
yrep <- matrix(rpois(length(mu), mu), nrow = nrow(mu))
observed <- replicated <- list()
for (k in c("Non-sexual", "Sexual")) {
  i <- which(dat$t == k)
  bw <- bw.nrd0(dat$y[i]); top <- max(dat$y[i], yrep[i, ])
  obs <- density(dat$y[i], bw = bw, from = 0, to = top, n = 512, cut = 0)
  observed[[k]] <- data.frame(x = obs$x, density = obs$y, outcome = k)
  replicated[[k]] <- data.frame(
    x = obs$x, outcome = k, replicate = rep(1:500, each = 512),
    density = as.vector(apply(yrep[i, ], 2, function(y)
      density(y, bw = bw, from = 0, to = top, n = 512, cut = 0)$y)))
}
p <- ggplot() +
  geom_line(data = do.call(rbind, replicated), aes(x, density, group = replicate),
            color = "grey65", alpha = 0.14, linewidth = 0.2) +
  geom_line(data = do.call(rbind, observed), aes(x, density), color = "grey10", linewidth = 0.75) +
  facet_wrap(~outcome, ncol = 1, scales = "free") +
  labs(x = "Counts per neighbourhood-year", y = "Density") + theme_paper(legend = "none")
save_fig("fig_ppc.png", p, 4.2, 5.0, DIR)

## hold-out validation
ho <- read.csv("results/tables/M4/holdout_predictions.csv")
ho$outcome <- ifelse(ho$type == "non_sexual", "Non-sexual", "Sexual")
p <- ggplot(ho, aes(observed, predicted)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_linerange(aes(ymin = lower, ymax = upper), color = "grey75", linewidth = 0.3, alpha = 0.6) +
  geom_point(aes(color = covered), size = 1, alpha = 0.75) +
  facet_wrap(~outcome, ncol = 1, scales = "free") +
  scale_color_manual(values = c(`TRUE` = "grey20", `FALSE` = "#c8664a"), name = NULL,
                     labels = c(`TRUE` = "Within 95% predictive interval", `FALSE` = "Outside")) +
  labs(x = "Observed count (held out)", y = "Predicted mean and 95% predictive interval") +
  theme_paper(legend = "top")
save_fig("fig_holdout.png", p, 4.2, 5.6, DIR)

## temporal factors by type
temporal <- read.csv("results/tables/M4/temporal_factors.csv")
temporal$type <- ifelse(temporal$type == "non_sexual", "Non-sexual", "Sexual")
p <- ggplot(temporal, aes(year, mean, color = type, fill = type)) +
  covid_mark(2020) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.13, color = NA) +
  geom_line(linewidth = LINE_W) + geom_point(size = PT_SZ) +
  scale_color_manual(values = COL_TYPE) + scale_fill_manual(values = COL_TYPE) +
  scale_x_continuous(breaks = 2018:2022) +
  scale_y_continuous(labels = function(v) sprintf("%.1f", v)) +
  labs(x = NULL, y = "Temporal component", title = "Time trend by type") +
  theme_paper(legend = "top")
save_fig("fig_temporal_type.png", p, 3.4, 2.6, DIR)

## relative-risk maps (2019, 2020, 2022 and all years)
lp <- fit$summary.linear.predictor
risk <- data.frame(dat[, c("cod_barrio", "year", "t")], RR = exp(lp$mean),
                   lo = exp(lp$`0.025quant`), hi = exp(lp$`0.975quant`))
sf_risk <- inner_join(carto, risk, by = "cod_barrio", relationship = "one-to-many")
lim <- as.numeric(quantile(c(risk$RR, 1 / risk$RR), 0.98))
rr_map <- function(d, title) {
  ggplot(d) + geom_sf(aes(fill = RR), color = "white", linewidth = 0.02) +
    facet_grid(year ~ t) +
    scale_fill_distiller(palette = "RdBu", direction = -1, trans = "log",
                         limits = c(1 / lim, lim), breaks = c(0.5, 1, 2, 4),
                         labels = c("0.5", "1", "2", "4"), name = "Relative\nrisk",
                         oob = scales::squish) +
    labs(title = title) + theme_map()
}
save_fig("fig_rr_maps.png", rr_map(filter(sf_risk, year %in% c(2019, 2020, 2022)),
         "Adjusted relative risk by period and type"), 6.8, 7.4, DIR)
save_fig("fig_rr_maps_full.png", rr_map(sf_risk, "Adjusted relative risk by year and type"),
         6.8, 11, DIR)

## relative risk in the four neighbourhoods of Fig 4
sel <- risk %>% filter(cod_barrio %in% names(SELECTED)) %>%
  mutate(neighbourhood = factor(SELECTED[cod_barrio], levels = SELECTED))
p <- ggplot(sel, aes(year, RR, color = t, fill = t, shape = t)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.35) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.13, color = NA) +
  geom_line(linewidth = LINE_W) + geom_point(size = PT_SZ) +
  facet_wrap(~neighbourhood, nrow = 2, scales = "free_y") +
  scale_color_manual(values = COL_TYPE) + scale_fill_manual(values = COL_TYPE) +
  scale_shape_manual(values = SHP_TYPE) + scale_x_continuous(breaks = c(2018, 2020, 2022)) +
  labs(x = NULL, y = "Adjusted relative risk",
       title = "Adjusted relative risk in four neighbourhoods") +
  theme_paper(legend = "top")
save_fig("fig_areas_fitted.png", p, 4.0, 3.1, DIR)

## shared field psi and non-sexual-specific field phi_1
comp <- rbind(
  data.frame(id_area = 1:285, effect = fit$summary.random$idx_psi1_pre$mean[1:285],
             component = "'Shared pattern'~(psi)"),
  data.frame(id_area = 1:285, effect = fit$summary.random$idx_phi1$mean[1:285],
             component = "'Non-sexual-specific'~(phi[1])"))
comp$component <- factor(comp$component, levels = unique(comp$component))
lc <- as.numeric(quantile(abs(comp$effect), 0.98))
p <- ggplot(inner_join(carto, comp, by = "id_area", relationship = "one-to-many")) +
  geom_sf(aes(fill = effect), color = "white", linewidth = 0.02) +
  facet_wrap(~component, nrow = 1, labeller = label_parsed) +
  scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(-lc, lc),
                       name = "log RR", oob = scales::squish) +
  labs(title = "Spatial decomposition of risk") + theme_map()
save_fig("fig_components.png", p, 8.5, 4.6, DIR)

## dual hotspots
exc <- read.csv("results/tables/M4/joint_exceedance.csv", colClasses = c(cod_barrio = "character"))
p <- ggplot(inner_join(carto, exc, by = "cod_barrio", relationship = "one-to-many")) +
  geom_sf(aes(fill = P_joint), color = "white", linewidth = 0.02) +
  facet_wrap(~year, ncol = 2) +
  scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(0, 1),
                       name = "Joint\nexceedance\nP(RR > 1)") +
  labs(title = "Dual hotspots") + theme_map()
save_fig("fig_hotspots.png", p, 7, 6.5, DIR)
