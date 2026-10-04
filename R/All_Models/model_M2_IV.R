################################################################################
# M2-IV: M1 plus a Type IV space-time interaction for each outcome, precision
# R_RW2 (x) R_ICAR, constrained to sum to zero over years within each area and
# over areas within each year (Goicoa et al., 2018; bigDM). Diagonal 1e-4
# regularises the remaining intrinsic directions.
# Takes about one hour and ends with numerical warnings; it enters no table.
################################################################################
source("R/All_Models/model_data.R")

Rt <- crossprod(diff(diag(n_year), differences = 2))
Rt <- Matrix(inla.scale.model(Rt, constr = list(A = rbind(rep(1, n_year), 1:n_year),
                                                e = c(0, 0))), sparse = TRUE)
W  <- sparseMatrix(i = rep(seq_len(n_area), g$nnbs), j = unlist(g$nbs), x = 1,
                   dims = c(n_area, n_area))
Rs <- inla.scale.model(Diagonal(n_area, rowSums(W)) - W,
                       constr = list(A = matrix(1, 1, n_area), e = 0))
R_IV <- as(kronecker(Rt, Rs), "CsparseMatrix")

## per-area and per-year sums; the global sum is added by constr = TRUE
C_area <- as.matrix(kronecker(Matrix(1, 1, n_year), Diagonal(n_area)))
C_year <- as.matrix(kronecker(Diagonal(n_year), Matrix(1, 1, n_area)))
A_IV   <- rbind(C_area[-1, , drop = FALSE], C_year[-1, , drop = FALSE])
C_IV   <- list(A = A_IV, e = rep(0, nrow(A_IV)))

formula <- y ~ 0 + resp_f +
  f(idx_psi_1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_psi_2, copy = "idx_psi_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_phi1, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_phi2, model = "bym2", graph = g, scale.model = TRUE, hyper = PC_BYM2) +
  f(idx_gam_1, model = "rw2", scale.model = TRUE, hyper = PC_PREC) +
  f(idx_gam_2, copy = "idx_gam_1", fixed = FALSE, hyper = PC_BETA) +
  f(idx_dev_2, model = "rw1", scale.model = TRUE, hyper = PC_PREC) +
  f(icell_1, model = "generic0", Cmatrix = R_IV, rankdef = 2 * n_area + n_year - 2,
    constr = TRUE, extraconstr = C_IV, diagonal = 1e-4, hyper = PC_PREC) +
  f(icell_2, model = "generic0", Cmatrix = R_IV, rankdef = 2 * n_area + n_year - 2,
    constr = TRUE, extraconstr = C_IV, diagonal = 1e-4, hyper = PC_PREC)

fit_model(formula, "M2_IV")
