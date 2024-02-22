test_that("Correctly returns True because two numbers are close enough", {
  expect_true(are_all_close(c(1, 1, 1), c(1, 1, 1)))
})

test_that("Correctly returns FALSE because the relative error is above rel_tol", {
  expect_false(are_all_close(c(0.1, 0.1, 0.1), c(0.1, 0.1, 0.1000002)))
})

test_that("Correctly returns FALSE because the absolute error is above abs_tol", {
  expect_false(are_all_close(c(10, 10, 10), c(10, 10, 10.00001)))
})
