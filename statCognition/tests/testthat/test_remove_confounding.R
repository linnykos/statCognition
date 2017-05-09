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
  gender <- as.factor(c("M", "F", "G", "M", "F"))
  pheno <- data.frame(age, gender)
  res <- .adjust_data_frame_regression(pheno)

  expect_true(is.data.frame(res))
  expect_true(all(dim(res) == c(5, 3)))
  expect_true(all(as.matrix(res[,2:3]) == as.matrix(.split_factors(pheno, 2))))
})

##################

## .compute_difference_pairings is correct

test_that(".compute_difference_pairings works", {
  mat <- matrix(1:30, 6, 5)
  pairings <- matrix(1:6, 3, 2)
  res <- .compute_difference_pairings(mat, pairings)

  expect_true(all(dim(res) == c(3,5)))
  expect_true(all(res == -3))
})

###################

## .construct_pairings_confounding is correct

test_that(".construct_pairings_confounding works", {
  mat <- matrix(c(1:10, 51:60, 101:110), 6, 5, byrow = T)
  dis <- as.matrix(stats::dist(scale(mat)))
  res <- .construct_pairings_confounding(dis)

  idx <- which(res == 1, arr.ind = T)[1]
  expect_true(all(sort(res[idx,]) == c(1,2)))

  idx <- which(res == 3, arr.ind = T)[1]
  expect_true(all(sort(res[idx,]) == c(3,4)))

  idx <- which(res == 5, arr.ind = T)[1]
  expect_true(all(sort(res[idx,]) == c(5,6)))
})

####################

## .regress_confounder is correct

test_that(".regress_confounder works", {
  set.seed(10)
  dat <- as.data.frame(matrix(1:30, 6, 5))
  vec <- stats::rnorm(6)
  mat <- cbind(vec, vec)

  res <- .regress_confounder(mat, dat)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(6,2)))
  expect_true(all(res[,1] == res[,2]))
})

########################

## .split_factors is correct

test_that(".split_factors works", {
  age <- 1:5
  gender <- as.factor(c("M", "F", "G", "M", "F"))
  pheno <- data.frame(age, gender)

  res <- .split_factors(pheno, 2)

  expect_true(is.data.frame(res))
  expect_true(all(dim(res) == c(5, 2)))

  #there's a correct encoding
  res2 <- as.matrix(res); vec <- res2[,1]+2*res[,2]; tab <- table(vec, gender)
  expect_true(all(apply(tab, 1, function(x){length(which(x != 0)) == 1})))
})

#####################

## .identify_factors is correct

test_that(".identify_factors works", {
  age <- 1:5
  gender <- c("M", "F", "M", "M", "F")
  gender2 <- as.factor(c("M", "F", "G", "M", "F"))
  pheno <- data.frame(age, gender, gender2)

  res <- .identify_factors(pheno)

  expect_true(res == 2)
})
