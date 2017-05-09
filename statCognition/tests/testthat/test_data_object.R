context("Test data object")

## data_object is correct

test_that("data_object works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  res <- data_object(list(mat = mat, pheno = pheno))
  expect_true(length(res) == 2)
  expect_true(class(res) == "data")
})

###############################

## get_primary is correct

test_that("get_primary works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))
  res <- get_primary(dat)

  expect_true(all(res == mat))
})
