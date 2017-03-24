context("Test criterion data correlation graph")

## criterion_data_monotonicity_pairs is correct

test_that("criterion_data_monotonicity_pairs works", {
  set.seed(10)
  x <- rnorm(100)
  mat <- cbind(x, x+0.1*rnorm(100), rnorm(100))
  res <- criterion_data_monotonicity_pairs(mat)

  expect_true(is.numeric(res))
})
