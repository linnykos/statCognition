context("Test state pheno")

## state_pheno_residual is correct

test_that("state_pheno_residual works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- state_pheno_residual(dat)

  expect_true(is.numeric(res))
})

#######################

## state_pheno_MI is correct

test_that("state_pheno_MI works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- state_pheno_MI(dat)

  expect_true(is.numeric(res))
})

#######################

## .l2norm is correct

test_that(".l2norm works", {
  res <- .l2norm(rep(1,10))
  expect_true(res == sqrt(10))
})

#####################

## .cut_sequence_bins is correct

test_that(".cut_sequence_bins works", {
  vec <- 1:100
  res <- .cut_sequence_bins(vec, 4)
  expect_true(length(levels(res)) == 4)
  expect_true(length(unique(res[1:25])) == 1)
  expect_true(length(unique(res[26:50])) == 1)
  expect_true(length(unique(res[51:75])) == 1)
  expect_true(length(unique(res[76:100])) == 1)
})

#####################

## .discretize_bins is correct

test_that(".discretize_bins works when pheno is a factor", {
  vec <- 1:100
  pheno <- as.factor(rep(c("M", "F"), each = 50))
  res <- .discretize_bins(vec, pheno, 4)

  expect_true(all(dim(res) == c(4,2)))
})

test_that(".discretize_bins works when pheno is not a factor", {
  vec <- 1:100
  pheno <- 1:100
  res <- .discretize_bins(vec, pheno, 4)

  expect_true(all(dim(res) == c(4,4)))
})

######################

## .predictive_error_linear_regression

test_that(".predictive_error_linear_regression works", {
  set.seed(10)
  vec <- rnorm(50)
  age <- rep(1:5, each=10)
  pheno <- data.frame(age)

  res <- .predictive_error_linear_regression(vec, pheno, vec, pheno)

  expect_true(length(res) == 50)
  expect_true(is.numeric(res))
})

######################

## .predictive_error_random_forest

test_that(".predictive_error_random_forest works", {
  set.seed(10)
  vec <- rnorm(50)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  res <- .predictive_error_random_forest(vec, pheno, vec, pheno)

  expect_true(length(res) == 50)
  expect_true(is.numeric(res))
})
