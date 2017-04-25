context("Test state data correlation graph")

## state_data_monotonicity_pairs is correct

test_that("state_data_monotonicity_pairs works", {
  set.seed(10)
  x <- rnorm(100)
  mat <- cbind(x, x+0.1*rnorm(100), rnorm(100))
  res <- state_data_monotonicity_pairs(mat)

  expect_true(is.numeric(res))
})
