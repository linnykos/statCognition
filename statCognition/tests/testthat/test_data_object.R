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

#############################

## is_valid.data is correct

test_that("is_valid.data works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))
  expect_true(is_valid(dat))
})

test_that("is_valid.data works with synthetic data", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()

  dat2 <- synthetic_generator(dat, init)
  expect_true(is_valid(dat2))
})

test_that("is_valid.data fails if seed is tampered", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()

  dat2 <- synthetic_generator(dat, init)
  dat2$synthetic_seed <- "asdf"
  expect_error(is_valid(dat2))
})

###########################

## .remove_idx is correct

test_that(".remove_idx works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  dat <- data_object(list(mat = mat, pheno = pheno))

  res <- .remove_idx(dat, c(1,5))

  expect_true(all(res$mat == dat$mat[2:4,]))
  expect_true(all(res$pheno == dat$pheno[2:4,]))
})
