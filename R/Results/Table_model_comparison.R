################################################################################
# Table: model comparison (WAIC, DIC, LS, effective number of parameters) and
# the CPU time reported by INLA for each fit.
# LS = -mean(log CPO); cells flagged by INLA keep their native CPO value.
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

models <- c("M0", "M1", "M2_I", "M2_II", "M2_III", "M2_IV",
            "M3_annual", "M3_l3", "SCM", "M5")

rows <- lapply(models, function(m) {
  path <- file.path("results/fits", paste0(m, ".rds"))
  if (!file.exists(path)) return(NULL)
  fit <- readRDS(path)
  data.frame(model = m, WAIC = fit$waic$waic, DIC = fit$dic$dic,
             LS = -mean(log(fit$cpo$cpo)), p_eff = fit$waic$p.eff,
             CPO_flagged = sum(fit$cpo$failure > 0, na.rm = TRUE),
             CPU_seconds = fit$cpu.used[["Total"]])
})
tab <- do.call(rbind, rows)

write.csv(tab, "results/tables/model_comparison.csv", row.names = FALSE)
print(format(tab, digits = 6), row.names = FALSE)
