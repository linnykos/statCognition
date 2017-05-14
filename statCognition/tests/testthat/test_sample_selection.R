context("Test sample selection")

## SS_none is correct

test_that("SS_none works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- SS_none(dat)

  expect_true(all(dat$mat == res$mat))
  expect_true(class(res) == "data")
})

#####################

## SS_neighborhood is correct

test_that("SS_neighborhood works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- SS_neighborhood(dat)

  expect_true(class(res) == "data")
})

#####################

## SS_cook is correct

test_that("SS_cook works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- 1:50
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), 10))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- SS_cook(dat)

  expect_true(class(res) == "data")
})
