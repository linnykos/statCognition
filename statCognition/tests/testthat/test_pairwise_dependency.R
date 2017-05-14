context("Test pairwise dependency")

## PD_pearson is correct

test_that("PD_pearson works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(stats::rnorm(100), 50, 2)))
  res <- PD_pearson(dat)
  expect_true(is.logical(res))
})

###################

## PD_kendall is correct

test_that("PD_kendall works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(stats::rnorm(100), 50, 2)))
  res <- PD_kendall(dat)
  expect_true(is.logical(res))
})

##################

## PD_energy is correct

test_that("PD_energy works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(stats::rnorm(100), 50, 2)))
  res <- PD_energy(dat)
  expect_true(is.logical(res))
})

