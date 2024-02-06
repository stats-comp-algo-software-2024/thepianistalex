test_that("are_all_close correctly handles 3 cases", {
  # Correctly returns True because two numbers are close enough
  expect_true(are_all_close(c(1,1,1), c(1,1,1), abs_tol = 1e-6, rel_tol = 1e-6))

  # Correctly returns FALSE because the relative error is above rel_tol
  expect_false(are_all_close(c(0.1,0.1,0.1), c(0.1,0.1,0.1000002), abs_tol = 1e-6, rel_tol = 1e-6))

  # Correctly returns FALSE because the absolute error is above abs_tol
  expect_false(are_all_close(c(10,10,10), c(10,10,10.00001), abs_tol = 1e-6, rel_tol = 1e-6))
})
