test_that("BFGS vs least_sq for linear model", {
  n_obs <- 100; n_pred <- 5
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 721)
  design <- data$design
  outcome <- data$outcome

  least_sq_out <- hiper_glm(design, outcome, model = 'linear', mle_finder = "least_sq")
  bfgs_out <- hiper_glm(design, outcome, model = 'linear', mle_finder = "BFGS")

  expect_true(are_all_close(coef(least_sq_out), coef(bfgs_out),
                            abs_tol = 1e-2, rel_tol = 1e-2))
})
