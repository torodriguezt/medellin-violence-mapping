################################################################################
# M2-II: M1 plus a Type II space-time interaction for each outcome, precision
# R_RW2 (x) I_A, with the constraints of Goicoa et al. (2018) as implemented in
# bigDM: the interaction sums to zero over years within each area, and gamma
# also has zero projection on a linear time trend. Diagonal 1e-4 regularises
# the remaining intrinsic directions.
# Takes about one hour and ends with numerical warnings; it enters no table.
################################################################################
source("R/All_Models/model_data.R")

Rt <- crossprod(diff(diag(n_year), differences = 2))
Rt <- Matrix(inla.scale.model(Rt, constr = list(A = rbind(rep(1, n_year), 1:n_year),
                                                e = c(0, 0))), sparse = TRUE)
R_II <- as(kronecker(Rt, Diagonal(n_area)), "CsparseMatrix")

## per-area sums over years; the global sum is added by constr = TRUE
C_area <- as.matrix(kronecker(Matrix(1, 1, n_year), Diagonal(n_area)))
C_II   <- list(A = C_area[-1, , drop = FALSE], e = rep(0, n_area - 1))
C_LIN  <- list(A = matrix(1:n_year, 1, n_year), e = 0)

formula <- y ~ 0 + resp_f +
  f(idx_psi_1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi_2, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC,
    constr = TRUE, extraconstr = C_LIN, rankdef = 2L, diagonal = 1e-4) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "generic0", Cmatrix = R_II, rankdef = 2 * n_area, constr = TRUE,
    extraconstr = C_II, diagonal = 1e-4, hyper = PC_PREC) +
  f(icell_2, model = "generic0", Cmatrix = R_II, rankdef = 2 * n_area, constr = TRUE,
    extraconstr = C_II, diagonal = 1e-4, hyper = PC_PREC)

fit_model(formula, "M2_II")
