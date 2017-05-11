context("Test synthetic generator functions: coupling")


## generator_monotonic is correct

test_that("generator_monotonic works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_monotonic(dat, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_monotonic works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_monotonic(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

###################

## generator_inflate_correlation is correct

test_that("generator_inflate_correlation works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_inflate_correlation(dat, 1, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_inflate_correlation works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_inflate_correlation(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

###################

## generator_cluster is correct

test_that("generator_cluster works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_cluster(dat, 1, 0.5, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_cluster works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_cluster(dat, 1e-4, 1e-4, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

###################

## generator_brownian is correct

test_that("generator_brownian works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_brownian(dat, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_brownian works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_brownian(dat, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

#######################

## generator_polynomial is correct

test_that("generator_polynomial works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_polynomial(dat, 1, 6, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_polynomial works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_polynomial(dat, 1e-4, 3.5, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

#######################

## generator_circle is correct

test_that("generator_circle works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_circle(dat, 1, 1, 1, 0.5, 0, 2*pi, pi/4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_circle works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_circle(dat, 1e-4, 1e-4, 1e-4, 0.5, 0, 1e-4,0)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

#######################

## generator_nearest_neighbor is correct

test_that("generator_nearest_neighbor works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_nearest_neighbor(dat, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_nearest_neighbor works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_nearest_neighbor(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})
