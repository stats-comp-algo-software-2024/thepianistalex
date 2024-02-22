#' Based on asymptotic distribution of log likelihood of MLE,
#' use the change in probability of chi squared statistic as criterion,
#' to check if a change in log likelihood is statistically large.
#' @noRd
check_statistical_convergence <- function(beta_old, beta_new, log_lik_null,
                                          p, design, outcome, rel_tol, abs_tol) {
  log_lik_old <- compute_logit_log_likelihood(beta_old, design, outcome)
  log_lik_new <- compute_logit_log_likelihood(beta_new, design, outcome)
  chisq_old <- 2 * (log_lik_old - log_lik_null)
  chisq_new <- 2 * (log_lik_new - log_lik_null)
  prob_old <- pchisq(chisq_old, p)
  prob_new <- pchisq(chisq_new, p)
  prob_change_abs <- abs(prob_old - prob_new)
  if ((prob_change_abs < abs_tol) & (prob_change_abs < rel_tol * prob_old)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
