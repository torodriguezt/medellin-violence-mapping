################################################################################
# Shared field psi of M4 under continuous hyperparameter integration. The 5,000
# joint hyperparameter draws behind Table 5 and Fig 8 (seed 511010) are passed
# to INLA as integration points (int.strategy = "user.expert"), in batches, and
# one Gaussian conditional draw of psi (improved mean, skew.corr = FALSE) is
# taken at each point, so that the field and the loadings of a draw stay
# paired. The article uses 5,000 draws in batches of 50. Completed batches are
# reused only with the same hyperparameter bank; changed banks are archived.
################################################################################
source("R/All_Models/model_data.R")
source("R/All_Models/model_M4_setup.R")
inla.setOption(num.threads = "4:1")
OUT <- "results/posterior/M4_psi_continuous"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

N <- 5000L
BATCH <- 50L

base <- readRDS(M4_FIT)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(511010)
H <- inla.hyperpar.sample(5000, base, intern = FALSE)[seq_len(N), , drop = FALSE]
tags <- rownames(base$summary.hyperpar)
stopifnot(identical(colnames(H), tags))
theta <- H
for (j in seq_along(tags)) theta[, j] <- base$misc$to.theta[[j]](H[, j])
stopifnot(all(is.finite(theta)))
bank <- file.path(OUT, "hyperparameters.rds")
if (file.exists(bank) && !identical(readRDS(bank), H)) {
  previous <- tempfile("M4_psi_previous_", tmpdir = dirname(OUT))
  if (!file.rename(OUT, previous)) stop("Could not archive the previous draw bank")
  dir.create(OUT, recursive = TRUE)
  message("Previous draw bank moved to ", previous)
}
saveRDS(H, bank)
mode_theta <- base$mode$theta
mode_x     <- base$mode$x
rm(base); invisible(gc())

for (first in seq.int(1L, N, by = BATCH)) {
  ids  <- first:min(N, first + BATCH - 1L)
  dest <- file.path(OUT, sprintf("draws_%05d_%05d.rds", first, max(ids)))
  if (file.exists(dest)) next
  t0 <- Sys.time()
  fit <- inla(m4, family = "poisson", data = dat, E = E,
              control.compute = list(config = TRUE),
              control.mode = list(theta = mode_theta, x = mode_x, restart = FALSE),
              control.inla = list(int.strategy = "user.expert",
                                  int.design = cbind(unname(theta[ids, , drop = FALSE]),
                                                     rep(1 / length(ids), length(ids)))))
  stopifnot(identical(rownames(fit$summary.hyperpar), tags))
  cs <- fit$misc$configs
  stopifnot(length(cs$config) == length(ids))
  psi <- matrix(NA_real_, 285L, length(ids))
  for (k in seq_along(cs$config)) {
    err <- apply(abs(sweep(theta[ids, , drop = FALSE], 2L, cs$config[[k]]$theta, "-")), 1L, max)
    pos <- which.min(err)
    stopifnot(err[pos] < 1e-6, all(is.na(psi[, pos])))
    one <- fit
    one$misc$configs$config  <- cs$config[k]
    one$misc$configs$nconfig <- 1L
    set.seed(720000L + ids[pos])
    s <- inla.posterior.sample(1L, one, selection = list(idx_psi1_pre = 1:285),
                               seed = 710000L + ids[pos], num.threads = "1:1",
                               parallel.configs = FALSE, use.improved.mean = TRUE,
                               skew.corr = FALSE)[[1L]]
    stopifnot(max(abs(s$hyperpar[tags] - H[ids[pos], ]) / (1 + abs(H[ids[pos], ]))) < 1e-6)
    psi[, pos] <- s$latent[paste0("idx_psi1_pre:", 1:285), 1]
  }
  stopifnot(all(is.finite(psi)))
  saveRDS(list(ids = ids, psi = psi), dest)
  message(sprintf("%s  draws %d-%d in %.0f s", format(Sys.time(), "%H:%M:%S"), first, max(ids),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  rm(fit, one, cs); invisible(gc())
}
