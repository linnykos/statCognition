context("Test remove confounders")

## RC_none is correct

test_that("RC_none works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- RC_none(dat)

  expect_true(difference_states_indicator(dat, res) == 0)
  expect_true(class(res) == "data")
})

#####################

## RC_linear_regression is correct

test_that("RC_linear_regression works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- RC_linear_regression(dat)

  expect_true(class(res) == "data")
})

#####################

## RC_pairing_difference is correct

test_that("RC_pairing_difference works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- RC_pairing_difference(dat)

  expect_true(class(res) == "data")
})

#######################

## .adjust_data_frame_regression is correct

test_that(".adjust_data_frame_regression works", {
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)
  res <- .adjust_data_frame_regression(pheno)

  expect_true(is.data.frame(res))
  expect_true(all(dim(res) == c(5, 2)))
})



