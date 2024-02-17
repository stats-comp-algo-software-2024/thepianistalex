#' Compute log likelihood for linear model
#' @noRd
compute_linear_log_likelihood <- function(beta, design, outcome, noise_var = 1) {

  fitted_values <- design %*% beta
  residuals <- outcome - fitted_values
  n <- length(outcome)
  log_lik <- -0.5 * n * log(2 * pi * noise_var) - (t(residuals) %*% residuals)/(2 * noise_var)


  return(log_lik)
}

#' Compute log likelihood gradient for linear model
#' @noRd
compute_linear_gradient <- function(beta, design, outcome, noise_var = 1) {

  gradient <- -(t(design) %*% design %*% beta - t(design) %*% outcome)/noise_var

  return(gradient)
}

#' Compute log likelihood for logit model
#' @noRd
compute_logit_log_likelihood <- function(beta, design, outcome) {

  pi <- 1/(1 + exp(-design %*% beta))
  log_lik <- outcome %*% log(pi) + (1 - outcome) %*% log(1 - pi)

  return(log_lik)
}

#' Compute log likelihood gradient for logit model
#' @noRd
compute_logit_gradient <- function(beta, design, outcome) {

  gradient <- t(design) %*% (outcome - 1/(1 + exp(-design %*% beta)))

  return(gradient)
}

#' Compute log likelihood hessian for logit model
#' @noRd
compute_logit_heassian <- function(beta, design, outcome) {
  pi <- 1/(1 + exp(-design %*% beta))
  W <- diag(pi * (1 - pi))
  hessian <- t(design) %*% W %*% design

  return(hessian)
}
