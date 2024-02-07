test_that("numerical vs analytics gradient for linear model", {
  n_obs <- 100
  n_pred <- 5
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 721)
  design <- data$design
  outcome <- data$outcome
  Beta <- data$coef_true

  # Compute the gradient using the analytical formula
  gradient_analytical <- compute_gradient(Beta, design, outcome)

  # Compute the gradient using the numerical formula
  gradient_numerical <- compute_numerical_grad(compute_log_likelihood, Beta, design, outcome)

  expect_true(are_all_close(gradient_analytical, gradient_numerical))
})
