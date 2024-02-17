#' Least square solver
#' @noRd
find_mle_linear <- function(design, outcome) {
  l <- chol(t(design) %*% design)
  tmp <- backsolve(l, t(design) %*% outcome, transpose = TRUE)
  beta <- backsolve(l, tmp)
  return(beta)
}

#' Newton’s method solver
#' @noRd
find_mle_newton <- function(design, outcome, beta_init = NULL, max_iter = 1000) {
  p <- ncol(design)
  if(is.null(beta_init)) {
    beta_init <- rep(0, p)
  }
  beta <- beta_init
  for(i in 1:max_iter) {
    gradient <- compute_logit_gradient(beta, design, outcome)
    hessian <- compute_logit_heassian(beta, design, outcome)
    beta <- beta + solve(hessian, gradient)
  }

  return(beta)
}

#' BFGS solver
#' @noRd
find_mle_bfgs <- function(design, outcome, model) {
  p <- ncol(design)
  if(model == "linear") {
    res <- stats::optim(rep(0, p), compute_linear_log_likelihood, compute_linear_gradient, design = design,
                        outcome = outcome, method = "BFGS", control = list(fnscale = -1))
  } else if(model == "logit") {
    res <- stats::optim(rep(0, p), compute_logit_log_likelihood, compute_logit_gradient, design = design,
                        outcome = outcome, method = "BFGS", control = list(fnscale = -1))
  }

  return(res$par)
}
