context("Test state data sample selection")

## state_data_influential_points is correct

test_that("state_data_influential_points works", {
  set.seed(10)
  mat <- matrix(rnorm(150), 15, 10)
  mat[5,] <- 20
  res <- state_data_influential_points(mat, num_pairs = 10)

  expect_true(res == 1)
})

###########################

## state_data_nearest_neighbor is correct

test_that("state_data_nearest_neighbor works", {
  set.seed(10)
  mat <- matrix(rnorm(150), 15, 10)
  mat[5,] <- 20
  res <- state_data_nearest_neighbor(mat)

  expect_true(res == 1)
})
