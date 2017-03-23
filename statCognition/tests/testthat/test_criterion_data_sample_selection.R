context("Test criterion data sample selection")

## criterionData_influential_points is correct

test_that("criterionData_influential_points works", {
  set.seed(10)
  mat <- matrix(rnorm(150), 15, 10)
  mat[5,] <- 20
  res <- criterionData_influential_points(mat, num_pairs = 10)

  expect_true(res == 1)
})

###########################

## criterionData_nearest_neighbor is correct

test_that("criterionData_nearest_neighbor works", {
  set.seed(10)
  mat <- matrix(rnorm(150), 15, 10)
  mat[5,] <- 20
  res <- criterionData_nearest_neighbor(mat)

  expect_true(res == 1)
})
