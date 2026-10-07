################################################################################
# Descriptive figures and M4 period loadings. PNG and PDF in figures/.
# Needs the processed panel, cartography and the M4 fit.
################################################################################
suppressPackageStartupMessages({library(INLA); library(sf); library(dplyr); library(ggplot2)})
source("R/Results/figure_style.R")
dir.create("figures", showWarnings = FALSE)

panel <- read.csv("results/data/panel.csv",
                  colClasses = c(cod_barrio = "character", cod_comuna = "character"))
panel$t <- ifelse(panel$type == "non_sexual", "Non-sexual", "Sexual")
carto <- readRDS("results/data/carto.rds")

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

## Spatial loadings by period and outcome (M4) and their ratio,
## from 5,000 joint hyperparameter draws (seed 511010)
m4 <- readRDS("results/fits/M3_l3_both_loadings_all.rds")
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(511010)
hs <- inla.hyperpar.sample(5000, m4, intern = FALSE)
ns <- cbind(1, hs[, "Beta for idx_psi1_conf"], hs[, "Beta for idx_psi1_post"])
sx <- hs[, paste0("Beta for idx_psi2_", c("pre", "conf", "post"))]
labels <- c("Pre-pandemic\n2018-2019", "Restriction\n2020-2021", "Post-restriction\n2022")
summ <- function(m, series, panel) data.frame(
  label = factor(labels, levels = labels), series = series, panel = panel,
  mean = colMeans(m), q025 = apply(m, 2, quantile, 0.025), q975 = apply(m, 2, quantile, 0.975))
d <- rbind(summ(ns, "Non-sexual", "Loading"), summ(sx, "Sexual", "Loading"),
           summ(sx / ns, "Ratio", "Ratio sexual / non-sexual"))
d[d$series == "Non-sexual" & d$label == labels[1], c("q025", "q975")] <- NA   # fixed at 1
p <- ggplot(d, aes(label, mean, color = series, shape = series)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.35) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.08, linewidth = 0.5,
                position = position_dodge(0.35), na.rm = TRUE) +
  geom_point(size = 1.9, position = position_dodge(0.35)) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(COL_TYPE, Ratio = "grey25"), breaks = c("Non-sexual", "Sexual"),
                     name = NULL) +
  scale_shape_manual(values = c(SHP_TYPE, Ratio = 18), breaks = c("Non-sexual", "Sexual"),
                     name = NULL) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = NULL, y = NULL, title = "Spatial loadings by period") +
  theme_paper(legend = "top")
save_fig("fig_coupling_periods.png", p, 4.2, 5.2, "figures")
