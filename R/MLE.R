#' Least square solver
#' @noRd
find_mle_linear <- function(design, outcome) {
  l <- chol(t(design) %*% design)
  tmp <- backsolve(l, t(design) %*% outcome, transpose = TRUE)
  mle <- backsolve(l, tmp)
  return(mle)
}

#' Newton’s method solver
#' @noRd
find_mle_newton <- function(design, outcome) {

  return(mle)
}

#' BFGS solver
#' @noRd
find_mle_bfgs <- function(design, outcome, model) {
  p <- ncol(design)
  if(model == "linear") {
    res <- stats::optim(rep(0, p), compute_linear_log_likelihood, compute_linear_gradient, design = design,
                        outcome = outcome, method = "BFGS", control = list(fnscale = -1))
  }

  return(res$par)
}
