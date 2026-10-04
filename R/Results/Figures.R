################################################################################
# All figures of the article, in the order they appear. PNG and PDF in figures/.
# Needs the M3 (l = 3) fit, posterior_draws.R, Table_hotspots.R and holdout.R.
################################################################################
suppressPackageStartupMessages({library(INLA); library(sf); library(dplyr); library(ggplot2)})
source("R/Results/figure_style.R")
dir.create("figures", showWarnings = FALSE)

panel <- read.csv("results/data/panel.csv",
                  colClasses = c(cod_barrio = "character", cod_comuna = "character"))
panel$t <- ifelse(panel$type == "non_sexual", "Non-sexual", "Sexual")
carto <- readRDS("results/data/carto.rds")
fit   <- readRDS("results/fits/M3_l3.rds")
dat   <- as.data.frame(fit$.args$data)
dat$t <- ifelse(dat$type == "non_sexual", "Non-sexual", "Sexual")
draws <- readRDS("results/posterior/M3_l3_draws.rds")

SELECTED <- c("0717" = "Robledo", "0607" = "Kennedy", "1412" = "San Lucas",
              "AUC2" = "San Antonio de Prado")

## Fig 1: communes and rural districts
carto$commune <- panel$cod_comuna[match(carto$cod_barrio, panel$cod_barrio)]
communes <- carto %>% group_by(commune) %>% summarise(.groups = "drop") %>%
  mutate(num = as.integer(commune))
p <- ggplot() +
  geom_sf(data = carto, aes(fill = commune), color = "white", linewidth = 0.10) +
  geom_sf(data = communes, fill = NA, color = "grey35", linewidth = 0.35) +
  geom_sf_label(data = st_point_on_surface(communes), aes(label = num), size = 3.1,
                color = "grey10", fontface = "bold", fill = "white", alpha = 0.75,
                linewidth = 0, label.padding = unit(0.09, "lines")) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "Communes of Medellín") + theme_map()
save_fig("fig01_region.png", p, 6.5, 7, "figures")

## Fig 2: crude rates, pooled 2018-2022
crude <- panel %>% group_by(cod_barrio, t) %>%
  summarise(rate = sum(y) / sum(pop) * 1e5, .groups = "drop")
p <- ggplot(inner_join(carto, crude, by = "cod_barrio", relationship = "one-to-many")) +
  geom_sf(aes(fill = rate), color = "white", linewidth = 0.04) +
  facet_wrap(~t) +
  scale_fill_distiller(palette = "Blues", direction = 1, trans = "sqrt", name = "Rate /100k") +
  labs(title = "Crude rates of violence against women") + theme_map()
save_fig("fig02_crude_maps.png", p, 10, 5.6, "figures")

## Fig 3: city-level crude rates by year
city <- panel %>% group_by(year, t) %>%
  summarise(rate = sum(y) / sum(pop) * 1e5, .groups = "drop")
p <- ggplot(city, aes(year, rate, color = t, shape = t)) +
  covid_mark(2020) +
  geom_line(linewidth = LINE_W) + geom_point(size = PT_SZ) +
  scale_color_manual(values = COL_TYPE) + scale_shape_manual(values = SHP_TYPE) +
  scale_x_continuous(breaks = 2018:2022) +
  labs(x = NULL, y = "Crude rate per 100,000 women",
       title = "Crude rates of violence against women") +
  theme_paper(legend = "top")
save_fig("fig03_crude_trend.png", p, 3.8, 2.6, "figures")

## Fig 4: crude rates in four neighbourhoods
sel <- panel %>% filter(cod_barrio %in% names(SELECTED)) %>%
  mutate(rate = y / pop * 1e5, neighbourhood = factor(SELECTED[cod_barrio], levels = SELECTED))
p <- ggplot(sel, aes(year, rate, color = t, shape = t)) +
  geom_line(linewidth = LINE_W) + geom_point(size = PT_SZ) +
  facet_wrap(~neighbourhood, nrow = 2, scales = "free_y") +
  scale_color_manual(values = COL_TYPE) + scale_shape_manual(values = SHP_TYPE) +
  scale_x_continuous(breaks = 2018:2022) +
  labs(x = NULL, y = "Crude rate per 100,000 women",
       title = "Crude rates in four neighbourhoods") +
  theme_paper(legend = "top")
save_fig("fig04_crude_areas.png", p, 4.2, 3.2, "figures")

## Fig 5: PIT histogram
pit <- data.frame(pit = fit$cpo$pit)
p <- ggplot(pit, aes(pit)) +
  geom_histogram(breaks = seq(0, 1, 0.05), fill = "grey83", color = "grey35", linewidth = 0.3) +
  geom_hline(yintercept = nrow(pit) / 20, linetype = "dashed", color = "grey45") +
  labs(x = "PIT value", y = "Count") + theme_paper(legend = "none")
save_fig("fig_pit.png", p, 4.2, 2.6, "figures")

## Fig 6: posterior predictive check, 500 replicated datasets
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
  facet_wrap(~outcome, nrow = 1, scales = "free") +
  labs(x = "Counts per neighbourhood-year", y = "Density") + theme_paper(legend = "none")
save_fig("fig_ppc.png", p, 7.2, 3.4, "figures")

## Fig 7: hold-out validation
ho <- read.csv("results/tables/holdout_predictions.csv")
ho$outcome <- ifelse(ho$type == "non_sexual", "Non-sexual", "Sexual")
p <- ggplot(ho, aes(observed, predicted)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_linerange(aes(ymin = lower, ymax = upper), color = "grey75", linewidth = 0.3, alpha = 0.6) +
  geom_point(aes(color = covered), size = 1, alpha = 0.75) +
  facet_wrap(~outcome, scales = "free") +
  scale_color_manual(values = c(`TRUE` = "grey20", `FALSE` = "#c8664a"), name = NULL,
                     labels = c(`TRUE` = "Within 95% predictive interval", `FALSE` = "Outside")) +
  labs(x = "Observed count (held out)", y = "Predicted mean and 95% predictive interval") +
  theme_paper(legend = "top")
save_fig("fig_holdout.png", p, 6.5, 3.6, "figures")

## Fig 8: relative spatial loading by period
beta   <- paste0("Beta for idx_psi2_", c("pre", "conf", "post"))
labels <- c("Pre-pandemic\n2018-2019", "Restriction\n2020-2021", "Post-restriction\n2022")
d <- data.frame(label = factor(labels, levels = labels),
                mean = fit$summary.hyperpar[beta, "mean"],
                q025 = fit$summary.hyperpar[beta, "0.025quant"],
                q975 = fit$summary.hyperpar[beta, "0.975quant"],
                res  = c(FALSE, TRUE, FALSE))
p <- ggplot(d, aes(label, mean)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.35) +
  geom_errorbar(aes(ymin = q025, ymax = q975, color = res), width = 0.06, linewidth = 0.5) +
  geom_point(aes(color = res, size = res)) +
  geom_text(aes(label = sprintf("%.2f", mean)), nudge_x = 0.14, hjust = 0, size = 3) +
  scale_color_manual(values = c(`TRUE` = COL_ACCENT, `FALSE` = "grey40"), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 2.2, `FALSE` = 1.6), guide = "none") +
  scale_x_discrete(expand = expansion(add = c(0.45, 0.65))) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = NULL, y = expression("Relative spatial loading " * delta),
       title = "Relative spatial loading by period") +
  theme_paper(legend = "none")
save_fig("fig_coupling_periods.png", p, 4.2, 2.9, "figures")

## Fig 9: temporal factors by type
x <- draws$other
ns <- t(sapply(1:5, function(k) exp(x["resp_fnon_sexual:1", ] + x[paste0("idx_gam_1:", k), ])))
sx <- t(sapply(1:5, function(k) exp(x["resp_fsexual:1", ] + x[paste0("idx_gam_2:", k), ] +
                                      x[paste0("idx_dev_2:", k), ])))
summ <- function(m, type) data.frame(year = 2018:2022, type = type, mean = rowMeans(m),
                                     q025 = apply(m, 1, quantile, 0.025),
                                     q975 = apply(m, 1, quantile, 0.975))
temporal <- rbind(summ(ns, "Non-sexual"), summ(sx, "Sexual"))
p <- ggplot(temporal, aes(year, mean, color = type, fill = type)) +
  covid_mark(2020) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.13, color = NA) +
  geom_line(linewidth = LINE_W) + geom_point(size = PT_SZ) +
  scale_color_manual(values = COL_TYPE) + scale_fill_manual(values = COL_TYPE) +
  scale_x_continuous(breaks = 2018:2022) +
  scale_y_continuous(labels = function(v) sprintf("%.1f", v)) +
  labs(x = NULL, y = "Temporal component", title = "Time trend by type") +
  theme_paper(legend = "top")
save_fig("fig_temporal_type.png", p, 3.4, 2.6, "figures")

## Fig 10 and Appendix: relative-risk maps (2019, 2020, 2022 and all years)
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
         "Adjusted relative risk by period and type"), 6.8, 7.4, "figures")
save_fig("fig_rr_maps_full.png", rr_map(sf_risk, "Adjusted relative risk by year and type"),
         6.8, 11, "figures")

## Fig 11: relative risk in the four neighbourhoods of Fig 4
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
save_fig("fig_areas_fitted.png", p, 4.0, 3.1, "figures")

## Fig 12: shared field psi and non-sexual-specific field phi_1
comp <- rbind(
  data.frame(id_area = 1:285, effect = fit$summary.random$idx_psi_1$mean[1:285],
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
save_fig("fig_components.png", p, 8.5, 4.6, "figures")

## Fig 13: dual hotspots
exc <- read.csv("results/tables/joint_exceedance.csv", colClasses = c(cod_barrio = "character"))
p <- ggplot(inner_join(carto, exc, by = "cod_barrio", relationship = "one-to-many")) +
  geom_sf(aes(fill = P_joint), color = "white", linewidth = 0.02) +
  facet_wrap(~year, ncol = 2) +
  scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(0, 1),
                       name = "Joint\nexceedance\nP(RR > 1)") +
  labs(title = "Dual hotspots") + theme_map()
save_fig("fig_hotspots.png", p, 7, 6.5, "figures")
