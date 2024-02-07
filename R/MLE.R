##### The high level mle function
find_mle <- function(design, outcome, model, option){
  if (option[["mle_solver"]] == "least_sq"){
    mle <- find_mle_least_sq(design, outcome)
  } else if(option[["mle_solver"]] == "BFGS") {
    mle <- find_mle_BFGS(design, outcome)
  }
  return(mle)
}

##### Least square solver
find_mle_least_sq <- function(design, outcome){
  R <- chol(t(design) %*% design)
  z <- backsolve(R, t(design) %*% outcome, transpose = TRUE)
  mle <- backsolve(R, z)
  return(mle)
}

##### BFGS solver
find_mle_BFGS <- function(design, outcome){
  p <- ncol(design)
  res <- stats::optim(rep(0,p),
                      compute_log_likelihood,
                      compute_gradient,
                      design = design, outcome = outcome,
                      method = "BFGS",control = list(fnscale = -1))
  return(res$par)
}

compute_log_likelihood <- function(Beta, design, outcome, noise_var=1) {
  fitted_values <- design %*% Beta
  residuals <- outcome - fitted_values
  n <- length(outcome)
  log_like <- -0.5*n*log(2*pi*noise_var) - (t(residuals)%*%residuals)/(2*noise_var)

  return(log_like)
}

compute_gradient <- function(Beta, design, outcome, noise_var=1) {
  gradient <- -1*(t(design)%*%design%*%Beta - t(design)%*%outcome)/noise_var

  return(gradient)
}
