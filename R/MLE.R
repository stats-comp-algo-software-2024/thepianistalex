#' Least square solver
#' @noRd
find_mle_linear <- function(design, outcome) {
  l <- chol(t(design) %*% design)
  tmp <- backsolve(l, t(design) %*% outcome, transpose = TRUE)
  mle <- backsolve(l, tmp)
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

#' Compute log likelihood for linear model
#' @noRd
compute_linear_log_likelihood <- function(beta, design, outcome, noise_var = 1) {

  fitted_values <- design %*% beta
  residuals <- outcome - fitted_values
  n <- length(outcome)
  log_like <- -0.5 * n * log(2 * pi * noise_var) - (t(residuals) %*% residuals)/(2 * noise_var)


  return(log_like)
}

#' Compute linear gradient for specified model
#' @noRd
compute_linear_gradient <- function(beta, design, outcome, noise_var = 1) {

  gradient <- -(t(design) %*% design %*% beta - t(design) %*% outcome)/noise_var

  return(gradient)
}
