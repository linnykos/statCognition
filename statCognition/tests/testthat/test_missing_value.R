context("Test missing value")

## MV_remove is correct

test_that("MV_remove works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  mat[sample(1:30, 3)] <- NA
  res <- MV_remove(mat)

  expect_true(is.matrix(res))
})

#####################

## MV_maximum_likelihood is correct

test_that("MV_maximum_likelihood works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  mat[sample(1:30, 3)] <- NA
  res <- MV_maximum_likelihood(mat)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(5,6)))
})

#####################

## MV_matching is correct

test_that("MV_matching works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  mat[sample(1:30, 3)] <- NA
  res <- MV_matching(mat)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(5,6)))
})

#####################

## MV_matrix_completion is correct

test_that("MV_matrix_completion works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  mat[sample(1:30, 3)] <- NA
  res <- MV_matrix_completion(mat)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(5,6)))
})


