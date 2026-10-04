# single visual system, sourced at the start of every figure script
# lockdown shown as a discreet thin line, not a shaded band; sober CVD-safe colours
library(ggplot2)

## palette: validated dE 10 CVD, meets the chroma floor
COL_TYPE <- c("Non-sexual" = "#c8664a",  # terracotta
              "Sexual"     = "#2a9d8f")  # teal
SHP_TYPE <- c("Non-sexual" = 16, "Sexual" = 17)

COL_ACCENT <- "#2a6f97"   # steel blue, to highlight one series
COL_MUTE   <- "grey65"

DIV_LOW  <- "#2a9d8f"; DIV_MID <- "grey96"; DIV_HIGH <- "#c8664a"

## marks: thin lines and small points, as in base R
## figures are drawn at roughly their printed size, so these are true widths
LINE_W <- 0.45
PT_SZ  <- 1.5

## theme for axis-based plots: close to base R graphics, no chrome
## black L-shaped axes with outward ticks, no grid, key inside the panel
theme_paper <- function(base = 10, legend = c("inside", "top", "none")) {
  legend <- match.arg(legend)
  th <- theme_classic(base_size = base) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(size = base + 1, color = "black", face = "bold",
                                hjust = 0.5, margin = margin(b = 7)),
      plot.subtitle = element_text(size = base - 1, color = "grey30",
                                   hjust = 0.5, margin = margin(b = 7)),
      axis.line  = element_line(color = "black", linewidth = 0.4),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(3.5, "pt"),
      axis.title = element_text(size = base - 1, color = "black"),
      axis.text  = element_text(size = base - 2, color = "black"),
      panel.grid = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title      = element_blank(),
      legend.text       = element_text(size = base - 2, color = "black"),
      legend.key.width  = unit(0.5, "cm"),
      legend.key.height = unit(0.42, "cm"),
      legend.margin     = margin(0, 0, 0, 0),
      strip.background = element_blank(),
      strip.text = element_text(size = base - 1, color = "black", face = "bold",
                                margin = margin(2, 2, 6, 2)),
      panel.spacing = unit(11, "pt"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 14, 8, 10)
    )
  th + switch(
    legend,
    inside = theme(legend.position = "inside",
                   legend.position.inside = c(0.01, 1),
                   legend.justification = c(0, 1)),
    top    = theme(legend.position = "top", legend.justification = "left"),
    none   = theme(legend.position = "none")
  )
}

## theme for maps
theme_map <- function(base = 12) {
  theme_void(base_size = base) +
    theme(
      plot.title = element_text(size = base + 3, color = "grey10",
                                face = "bold", margin = margin(b = 3)),
      plot.subtitle = element_text(size = base, color = "grey40",
                                   margin = margin(b = 8)),
      plot.title.position = "plot",
      legend.position = "right",
      legend.title = element_text(size = base - 1, color = "grey35"),
      legend.text  = element_text(size = base - 1, color = "grey40"),
      legend.key.height = unit(0.9, "cm"),
      strip.text = element_text(size = base + 1, color = "grey15",
                                face = "bold", margin = margin(4, 4, 7, 4)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 12, 12, 12)
    )
}

## discreet lockdown mark: thin dashed line + label, as abline(lty = 2)
covid_mark <- function(x = 2020, label = TRUE, size = 2.8) {
  ## geom_vline, not an annotated segment: spans the panel on any y scale
  out <- list(
    geom_vline(xintercept = x, linetype = "22",
               color = "grey45", linewidth = 0.35)
  )
  if (label) out <- c(out, list(
    annotate("text", x = x, y = Inf, label = "COVID-19",
             vjust = 1.4, hjust = -0.08, size = size, color = "grey35")))
  out
}

## saves a PNG (preview) and a vector PDF (article); cairo_pdf embeds unicode (psi, phi, delta)
save_fig <- function(nombre, plot, w, h, dir_fig) {
  ggsave(file.path(dir_fig, nombre), plot, width = w, height = h,
         dpi = 320, bg = "white")
  pdf_name <- sub("\\.png$", ".pdf", nombre)
  ggsave(file.path(dir_fig, pdf_name), plot, width = w, height = h,
         bg = "white", device = cairo_pdf)
}
