################################################################################
# M3 reference for the article's comparison with M4. Joint exceedance for
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
