test_that("numerical vs analytics gradient for linear model", {
  n_obs <- 100
  n_pred <- 5
  model <- "linear"
  data <- simulate_data(n_obs, n_pred, model = model, seed = 721)
  design <- data$design
  outcome <- data$outcome
  beta <- data$coef_true

  gradient_analytical <- compute_linear_gradient(beta, design, outcome)
  gradient_numerical <- compute_numerical_grad(compute_linear_log_likelihood, beta, design, outcome)

  expect_true(are_all_close(gradient_analytical, gradient_numerical))
})

test_that("numerical vs analytics gradient for logit model", {
  n_obs <- 100
  n_pred <- 5
  model <- "logit"
  data <- simulate_data(n_obs, n_pred, model = model, seed = 721)
  design <- data$design
  outcome <- data$outcome
  beta <- data$coef_true

  gradient_analytical <- compute_logit_gradient(beta, design, outcome)
  gradient_numerical <- compute_numerical_grad(compute_logit_log_likelihood, beta, design, outcome)

  expect_true(are_all_close(gradient_analytical, gradient_numerical))
})
