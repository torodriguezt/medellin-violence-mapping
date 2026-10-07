# Starting-value rows reported in article_public_health.tex (tab:appA_starts).
# Only the main M4 data: sensitivity variants are fitted in M4_sensitivity.R.
source("R/All_Models/model_data.R")
source("R/All_Models/model_M4_setup.R")
dir.create("results/tables/M4_starts", recursive = TRUE, showWarnings = FALSE)

## One common initial value for the five free loadings
STARTS <- list(`0` = list(init = 0, restart = FALSE), `0.5` = list(init = 0.5, restart = FALSE),
               `1` = list(init = 1, restart = FALSE), `1 restart` = list(init = 1, restart = TRUE),
               `-1` = list(init = -1, restart = FALSE))

## variant = list(data, formula, loading prior, starting values)
VARIANTS <- list(
  all           = list("all", m4, c(0, 1), c(STARTS, list(`0 repeat` = list(init = 0, restart = FALSE)))),
  all_restart   = list("all", m4, c(0, 1), list(`0 restart` = list(init = 0, restart = TRUE),
                    `0.5 restart` = list(init = 0.5, restart = TRUE),
                    `1.5 restart` = list(init = 1.5, restart = TRUE),
                    `1 restart repeat` = list(init = 1, restart = TRUE))))

todo <- commandArgs(trailingOnly = TRUE)
if (length(todo) == 0) todo <- names(VARIANTS)
stopifnot(all(todo %in% names(VARIANTS)))

for (v in todo) {
  spec <- VARIANTS[[v]]
  d <- m4_data(spec[[1]])
  LOAD_PRIOR <- spec[[3]]
  rows <- list()
  for (s in names(spec[[4]])) {
    LOAD_INIT <- rep_len(spec[[4]][[s]]$init, 5)
    t0 <- Sys.time()
    fit <- if (v == "all" && s == "0") readRDS(M4_FIT) else
      fit_m4(spec[[2]], d, restart = spec[[4]][[s]]$restart)
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(511010)
    hs <- inla.hyperpar.sample(5000, fit, intern = FALSE)
    cs <- m4_contrasts(hs)
    q  <- apply(cs, 2, quantile, c(0.025, 0.975))
    load_means <- fit$summary.hyperpar[BETA, "mean"]
    phi2 <- grep("^Precision for idx_phi2", rownames(fit$summary.hyperpar), value = TRUE)
    status <- c(fit$mode$mode.status, fit$misc$mode.status, NA)[1]
    rows[[s]] <- data.frame(
      variant = v, start = s, restart = spec[[4]][[s]]$restart,
      mlik = fit$mlik[1, 1], WAIC = fit$waic$waic, DIC = fit$dic$dic,
      mode_status = status, seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
      t(setNames(load_means, c("ns_res", "ns_post", "sx_pre", "sx_res", "sx_post"))),
      sigma_phi2 = 1 / sqrt(fit$summary.hyperpar[phi2, "0.5quant"]),
      t(setNames(colMeans(cs), colnames(cs))),
      t(setNames(q[1, ], paste0(colnames(cs), "_q025"))),
      t(setNames(q[2, ], paste0(colnames(cs), "_q975"))),
      t(setNames(colMeans(cs > 0), paste0(colnames(cs), "_P"))), row.names = NULL)
    message(sprintf("%s | start %s: mlik %.2f, WAIC %.1f, loadings %s, ratio res-pre %.3f",
                    v, s, fit$mlik[1, 1], fit$waic$waic,
                    paste(sprintf("%.3f", load_means), collapse = " "),
                    mean(cs[, "ratio_res_pre"])))
    write.csv(do.call(rbind, rows), file.path("results/tables/M4_starts", paste0(v, ".csv")),
              row.names = FALSE)
    rm(fit); invisible(gc())
  }
}
