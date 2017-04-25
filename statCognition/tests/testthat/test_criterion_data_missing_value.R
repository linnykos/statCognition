context("Test criterion data missing values")

############################

## criterion_data_residual_RF_LR is correct

test_that("criterion_data_residual_RF_LR works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  res <- criterion_data_residual_RF_LR(mat)

  expect_true(is.numeric(res))
})
