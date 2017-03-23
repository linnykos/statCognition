context("Test remove confounders")

## RC_none is correct

test_that("RC_none works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  res <- RC_none(mat, pheno)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(5,6)))
})

#####################

## RC_linear_regression is correct

test_that("RC_linear_regression works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)
  res <- RC_linear_regression(mat, pheno)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(5,6)))
})

#####################

## RC_pairing_difference is correct

test_that("RC_pairing_difference works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)
  res <- RC_pairing_difference(mat, pheno)

  expect_true(is.matrix(res))
})

# #####################
#
# ## RC_kernel_regression is correct
#
# test_that("RC_kernel_regression works", {
#   set.seed(10)
#   mat <- matrix(rnorm(30), 5, 6)
#   age <- 1:5
#   gender <- as.factor(c("M", "F", "M", "M", "F"))
#   pheno <- data.frame(age, gender)
#   res <- RC_kernel_regression(mat, pheno)
#
#   expect_true(is.matrix(res))
#   expect_true(all(dim(res) == c(5,6)))
# })
#
#
