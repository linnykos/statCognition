context("Test difference states")

## difference_states_indicator is correct

test_that("difference_states_indicator works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat_1 <- data_object(list(mat = mat, pheno = pheno))
  dat_2 <- data_object(list(mat = mat, pheno = pheno))

  expect_true(difference_states_indicator(dat_1, dat_2) == 0)
})

test_that("difference_states_indicator can return 0", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  mat2 <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat_1 <- data_object(list(mat = mat, pheno = pheno))
  dat_2 <- data_object(list(mat = mat2, pheno = pheno))

  expect_true(difference_states_indicator(dat_1, dat_2) == 1)
})

################

## all.equal.matrix is correct

test_that("all.equal.matrix works", {
  mat <- matrix(1:20, 4, 5)

  expect_true(all.equal.matrix(mat, mat))
})

test_that("all.equal.matrix returns false for different sized matrices", {
  mat <- matrix(1:20, 4, 5)
  mat2 <- matrix(1:25, 5, 5)

  expect_true(!all.equal.matrix(mat, mat2))
})

test_that("all.equal.matrix returns false for same dim matrices but diff values", {
  mat <- matrix(1:20, 4, 5)
  mat2 <- matrix(21:40, 4, 5)

  expect_true(all(dim(mat) == dim(mat2)))
  expect_true(!all.equal.matrix(mat, mat2))
})

test_that("all.equal.matrix is called by default", {
  mat <- matrix(1:20, 4, 5)
  mat2 <- matrix(21:40, 4, 5)

  expect_true(!all.equal(mat, mat2))
})
