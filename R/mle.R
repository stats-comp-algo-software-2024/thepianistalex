#' least square solver
#' @noRd
find_mle_linear <- function(design, outcome) {
  l <- chol(t(design) %*% design)
  tmp <- backsolve(l, t(design) %*% outcome, transpose = TRUE)
  beta <- backsolve(l, tmp)
  return(beta)
}

#' newton’s method solver
#' @noRd
find_mle_newton <- function(design, outcome, beta_init = NULL, max_iter = 100,
                            abs_tol = 1e-6, rel_tol = 1e-6) {
  p <- ncol(design)
  if (is.null(beta_init)) {
    beta_init <- rep(0, p)
  }
  log_lik_null <- compute_logit_log_likelihood(rep(0, p), design, outcome)
  beta_old <- beta_init
  converged <- FALSE
  for (i in 1:max_iter) {
    gradient <- compute_logit_gradient(beta_old, design, outcome)
    hessian <- compute_logit_heassian(beta_old, design, outcome)
    beta_new <- beta_old + solve(hessian, gradient)
    converged <- check_statistical_convergence(
      beta_old, beta_new, log_lik_null,
      p, design, outcome, rel_tol, abs_tol
    )
    if (converged) {
      break
    } else {
      beta_old <- beta_new
    }
  }

  if (!converged) {
    warning(paste("Newton’s method did not converge after", max_iter, "iterations"))
  }

  return(beta_new)
}

#' bfgs solver
#' @noRd
find_mle_bfgs <- function(design, outcome, model) {
  p <- ncol(design)
  if (model == "linear") {
    res <- stats::optim(rep(0, p), compute_linear_log_likelihood, compute_linear_gradient,
      design = design,
      outcome = outcome, method = "BFGS", control = list(fnscale = -1)
    )
  } else if (model == "logit") {
    res <- stats::optim(rep(0, p), compute_logit_log_likelihood, compute_logit_gradient,
      design = design,
      outcome = outcome, method = "BFGS", control = list(fnscale = -1)
    )
  }

  return(res$par)
}
