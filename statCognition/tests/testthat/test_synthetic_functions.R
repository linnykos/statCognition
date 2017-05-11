context("Test synthetic generator functions")

## generator_refit_normality is correct

test_that("generator_refit_normality works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_refit_normality(dat, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_refit_normality works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_refit_normality(dat, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

####################

## generator_resample is correct

test_that("generator_resample works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_resample(dat, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_resample works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_resample(dat, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

##################

## generator_resample_pheno is correct

test_that("generator_resample_pheno works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  res <- generator_resample_pheno(dat, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_resample_pheno works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  res <- generator_resample_pheno(dat, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

##################

## generator_add_noise is correct

test_that("generator_add_noise works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_add_noise(dat, 1, 1, 1, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_add_noise works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_add_noise(dat, 1e-4, 1e-4, 1e-4, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

##################

## generator_shuffle is correct

test_that("generator_shuffle works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_shuffle(dat, 1, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_shuffle works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_shuffle(dat, 1e-4, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

##################

## generator_decouple_empirical is correct

test_that("generator_decouple_empirical works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_decouple_empirical(dat, 1, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_decouple_empirical works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_decouple_empirical(dat, 1e-4, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

##################

## generator_monotonic is correct

test_that("generator_monotonic works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_monotonic(dat, 1, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_monotonic works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_monotonic(dat, 1e-4, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

###################

## generator_inflate_correlation is correct

test_that("generator_inflate_correlation works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_inflate_correlation(dat, 1, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_inflate_correlation works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_inflate_correlation(dat, 1e-4, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

###################

## generator_cluster is correct

test_that("generator_cluster works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_cluster(dat, 1, 1, 1, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_cluster works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_cluster(dat, 1e-4, 1e-4, 1e-4, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

###################

## generator_brownian is correct

test_that("generator_brownian works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_brownian(dat, 1)

  expect_true(all(dim(res) == c(6,5)))
})

test_that("generator_brownian works with 0", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  res <- generator_brownian(dat, 1e-4)

  expect_true(all(dim(res) == c(6,5)))
})

