context("Test synthetic generator functions: coupling")


## generator_monotonic is correct

test_that("generator_monotonic works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_monotonic(dat, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

test_that("generator_monotonic works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_monotonic(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

###################

## generator_inflate_correlation is correct

test_that("generator_inflate_correlation works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_inflate_correlation(dat, 1, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

test_that("generator_inflate_correlation works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_inflate_correlation(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

###################

## generator_cluster is correct

test_that("generator_cluster works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_cluster(dat, 1, 0.5, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

test_that("generator_cluster works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_cluster(dat, 1e-4, 1e-4, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

###################

## generator_brownian is correct

test_that("generator_brownian works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_brownian(dat, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

test_that("generator_brownian works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_brownian(dat, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == c(6,5)))
})

