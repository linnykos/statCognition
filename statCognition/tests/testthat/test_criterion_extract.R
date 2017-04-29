context("Test state extracting")

## cognition_state is correct

test_that("cognition_state works", {
  clist <- cognition_state()

  expect_true(is.list(clist))
})

test_that("cognition_state is a list of functions", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  clist <- cognition_state()
  res <- clist[[1]](mat, pheno = pheno)

  expect_true(is.numeric(res))
})

###################

## .apply_cognition_state is correct

test_that(".apply_cognition_state works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  clist <- cognition_state()
  res <- .apply_cognition_state(clist, mat, pheno)

  expect_true(is.numeric(res))
  expect_true(!is.matrix(res))
})
