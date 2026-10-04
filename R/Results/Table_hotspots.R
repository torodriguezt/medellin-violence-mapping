################################################################################
# Table: dual hotspots. Joint exceedance P(RR_ns > 1 and RR_sx > 1 | y) for
# every neighbourhood-year from 1,000 joint draws of M3 (l = 3), drawn
# uniformly (seed 532027) from the 2,000 saved by posterior_draws.R.
################################################################################
suppressPackageStartupMessages(library(INLA))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

fit   <- readRDS("results/fits/M3_l3.rds")
draws <- readRDS("results/posterior/M3_l3_draws.rds")
dat   <- as.data.frame(fit$.args$data)
ns <- which(dat$resp == 1); sx <- which(dat$resp == 2)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(532027)
cols <- sort(sample.int(ncol(draws$eta), 1000))
exc_ns <- draws$eta[ns, cols] > 0
exc_sx <- draws$eta[sx, cols] > 0

cells <- data.frame(dat[ns, c("cod_barrio", "nombre_barrio", "nombre_comuna", "year")],
                    RR_ns = exp(fit$summary.linear.predictor$mean[ns]),
                    RR_sx = exp(fit$summary.linear.predictor$mean[sx]),
                    P_ns = rowMeans(exc_ns), P_sx = rowMeans(exc_sx),
                    P_joint = rowMeans(exc_ns & exc_sx), row.names = NULL)
write.csv(cells, "results/tables/joint_exceedance.csv", row.names = FALSE)

## persistent hotspots: joint exceedance in all 1,000 draws in all five years
by_area <- split(cells, cells$cod_barrio)
persistent <- do.call(rbind, lapply(by_area, function(d) {
  if (min(d$P_joint) < 1) return(NULL)
  data.frame(d[1, c("cod_barrio", "nombre_barrio", "nombre_comuna")],
             RR_ns = mean(d$RR_ns), RR_sx = mean(d$RR_sx))
}))
write.csv(persistent, "results/tables/persistent_hotspots.csv", row.names = FALSE)
print(persistent, row.names = FALSE)

## neighbourhoods with P_joint > 0.8, by year
print(tapply(cells$P_joint > 0.8, cells$year, sum))

## joint vs product of marginals where both marginals lie in (0.05, 0.95)
u <- subset(cells, P_ns > 0.05 & P_ns < 0.95 & P_sx > 0.05 & P_sx < 0.95)
ratio <- u$P_joint / (u$P_ns * u$P_sx)
message(sprintf("Joint / product of marginals (n = %d): mean %.3f, max %.2f",
                nrow(u), mean(ratio), max(ratio)))
