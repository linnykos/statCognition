context("Test criterion data missing values")

## criterionData_evenness_missing is correct

test_that("criterionData_evenness_missing works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  mat[sample(1:30, 3)] <- NA
  res <- criterionData_evenness_missing(mat)

  expect_true(is.numeric(res))
})

############################

## criterionData_residual_RF_LR is correct

test_that("criterionData_residual_RF_LR works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  mat[sample(1:30, 3)] <- NA
  res <- criterionData_residual_RF_LR(mat)

  expect_true(is.numeric(res))
})
