# =====================================================
# inla_rgeneric_scm_change_typeI.R
# -----------------------------------------------------
# PATCHED version of the rgeneric from Retegui, Etxeberria and Ugarte
# (2024), taken from https://github.com/spatialstatisticsupna/Shared_interactions
# (R/All_Models/Multivariate_Models_shared/2_SCM_T/inla_rgeneric_scm_change_typeI.R)
#
# THE MODEL IS NOT MODIFIED. Only namespaces are qualified:
# Diagonal()/bdiag() with Matrix:: and inla.as.sparse() with INLA::.
# Verified against upstream commit a0997e5245a5da31244eb13db627e2755797c76c,
# source blob a22ab42fbb2150cd61c9f1c0086e050cf6f973ce.
#
# PARAMETERISATION (also checked against the saved SCM and M5 prior matrices):
# theta[1] = log(tau_chi); theta[t+1] = log(rho_t).
# First block z1 = rho_t * chi_it; second block z2 = chi_it/rho_t + epsilon.
# Hence z2 = rho_t^-2 * z1 + epsilon and the relative loading is rho_t^-2.
# The finite-copy error has precision exp(15). Shape-rate priors are
# tau_chi ~ Gamma(1, 5e-5), rho_t ~ Gamma(10,10), with log-scale Jacobians.
#
# WHY THE PATCH IS NEEDED
# The `inla` binary evaluates the rgeneric in a clean R subprocess
# that loads INLA but does NOT attach the Matrix package. In INLA
# 24.12.11 that makes `Diagonal()` unresolved and the fit fails with:
#
#     *** ERROR *** rgeneric [graph] with model [...] failed
#     Error in Diagonal(x = rep(1, nrow(W))) :
#
# The error stays hidden unless the fit is run with verbose=TRUE and
# safe=FALSE. In earlier INLA versions Matrix remained attached in
# that subprocess, so the original code worked as-is.
# =====================================================

inla.rgeneric.SCM = function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const",
            "log.prior", "quit"),
    theta = NULL){

  envir = parent.env(environment())

  interpret.theta = function(){
    return(
      list(prec = exp(theta[1L]),
           a = sapply(theta[as.integer(2:(k+1))], function(x){exp(x)})
      )
    )

  }

  graph = function(){
    return(Q())
  }

  Q = function(){
    param = interpret.theta()

    C0 <- Matrix::Diagonal(x = rep(1,nrow(W)))
    I <- Matrix::Diagonal(x = rep(1,k))
    C1 <- kronecker(I,C0)

    delta <- kronecker(solve(Matrix::Diagonal(x = param$a)),
                       Matrix::Diagonal(x = rep(1,nrow(W))))
    delta2 <- delta %*% delta
    prec_z <- delta %*% C1 %*% delta

    C <- Matrix::bdiag(param$prec * prec_z +
                 exp(15) * delta2 %*% C1 %*% delta2,
               exp(15) * C1)
    C[1:(k*nrow(W)),(k*nrow(W)+1):(nrow(W)*k*2)] <- - exp(15) * delta2 %*% C1
    C[(k*nrow(W)+1):(nrow(W)*k*2), 1:(k*nrow(W))] <- - exp(15) * C1 %*% delta2

    Q <- INLA::inla.as.sparse(C)
    return(Q)
  }

  mu = function(){
    return(numeric(0))
  }

  log.norm.const = function(){
    return(numeric(0))
  }

  log.prior = function(){
    param = interpret.theta()

    res <- dgamma(param$prec, 1, 5e-05, log = TRUE) + theta[1L] +
      sum(dgamma(param$a, 10, 10, log = TRUE)) + sum(theta[2:(k+1)])

    return(res)
  }

  initial = function(){
    return(as.vector(initial.values))
  }

  quit = function(){
    return(invisible())
  }

  if (!length(theta)){
    theta <- initial()
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}
