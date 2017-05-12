context("Test synthetic generator functions: decoupling")

## generator_refit_normality is correct

test_that("generator_refit_normality works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_refit_normality(dat, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_refit_normality works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_refit_normality(dat, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

####################

## generator_resample is correct

test_that("generator_resample works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_resample(dat, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_resample works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_resample(dat, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_add_noise is correct

test_that("generator_add_noise works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_add_noise(dat, 1, 0.5, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_add_noise works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_add_noise(dat, 1e-4, 1e-4, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_shuffle is correct

test_that("generator_shuffle works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_shuffle(dat, 1, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_shuffle works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_shuffle(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_decouple_empirical is correct

test_that("generator_decouple_empirical works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_decouple_empirical(dat, 1, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_decouple_empirical works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_decouple_empirical(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_outlier is correct

test_that("generator_outlier works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_outlier(dat, 5, 4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_outlier works with small values", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_outlier(dat, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})
