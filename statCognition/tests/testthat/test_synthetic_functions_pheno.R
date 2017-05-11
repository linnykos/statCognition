context("Test synthetic generator functions: phenotype")

## generator_resample_pheno is correct

test_that("generator_resample_pheno works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_resample_pheno(dat, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_resample_pheno works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_resample_pheno(dat, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_linearize_pheno is correct

test_that("generator_linearize_pheno works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_linearize_pheno(dat, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_linearize_pheno works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_linearize_pheno(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_monotonic_pheno is correct

test_that("generator_monotonic_pheno works", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_monotonic_pheno(dat, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_monotonic_pheno works with 0", {
  set.seed(10)
  load("../assets/demo.RData")
  res <- generator_monotonic_pheno(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})
