context("Test estimating monotone piecewise marginals")

## .sort_matrix is correct

test_that(".sort_matrix works", {
  mat <- matrix(1:30,6,5); idx <- rep(4,6)
  res <- .sort_matrix(mat, idx)

  expect_true(is.list(res))
  expect_true(all(dim(res$mat) == dim(mat)))
  expect_true(length(res$idx) == length(idx))
})

test_that(".sort_matrix properly sorts each row", {
  mat <- matrix(30:1,6,5); idx <- rep(4,6)
  res <- .sort_matrix(mat, idx)

  expect_true(all(res$mat == matrix(1:30,6,5)[6:1,]))
})

test_that(".sort_matrix properly updates the idx", {
  set.seed(10)
  mat <- matrix(sample(1:30),6,5); idx <- sample(1:5,6,replace = T)
  res <- .sort_matrix(mat, idx)

  for(i in 1:6){
    expect_true(res$mat[i,res$idx[i]] == mat[i,idx[i]])
  }
})

##########################

## .form_lp_constraint_byrow is correct

test_that(".form_lp_constraint_byrow works", {
  val <- c(10:15); idx <- 3; n <- 6
  htable <- hash::hash(keys = as.character(val), values = 1:length(val))
  val <- c(val, idx)

  res <- .form_lp_constraint_byrow(val, htable, n)

  expect_true(length(res) == n)
  expect_true(all(res == c(0,0,-1,0,0,1)))
})

test_that(".form_lp_constraint_byrow handles if idx is the last element", {
  val <- c(10:15); idx <- 6; n <- 6
  htable <- hash::hash(keys = as.character(val), values = 1:length(val))
  val <- c(val, idx)

  res <- .form_lp_constraint_byrow(val, htable, n)

  expect_true(length(res) == 0)
})

test_that(".form_lp_constraint_byrow handles if idx is the first element", {
  val <- c(10:15); idx <- 1; n <- 6
  htable <- hash::hash(keys = as.character(val), values = 1:length(val))
  val <- c(val, idx)

  res <- .form_lp_constraint_byrow(val, htable, n)

  expect_true(length(res) == n)
  expect_true(all(res == c(-1,0,0,0,0,1)))
})

###########################

## .form_monotonic_constraints is correct

test_that(".form_monotonic_constraints works", {
  res <- .form_monotonic_constraints(5)
  expect_true(all(dim(res) == c(4,5)))
  for(i in 1:4){
    vec <- rep(0,5)
    vec[c(i,i+1)] <- c(-1,1)
    expect_true(all(res[i,] == vec))
  }
})

## .form_bounded_above_constraint is correct

test_that(".form_bounded_above_constraint works", {
  res <- .form_bounded_above_constraint(5)
  expect_true(all(res == c(0,0,0,0,1)))
})


## .form_bounded_below_constraint is correct

test_that(".form_bounded_below_constraint works", {
  res <- .form_bounded_below_constraint(5)
  expect_true(all(res == c(-1,0,0,0,0)))
})

###########################

## .form_lp_obj is correct

test_that(".form_lp_obj works", {
  mat <- matrix(1:30,6,5); idx <- rep(3,6); n <- 30
  val <- as.numeric(mat)
  htable <- hash::hash(keys = as.character(val), values = 1:length(val))
  res <- .form_lp_obj(mat, idx, htable, n)

  expect_true(all(res == c(rep(0,6), rep(-1,6), rep(1, 6), rep(0, 12))))
})

test_that(".form_lp_obj works when one element of idx is 1", {
  mat <- matrix(1:30,6,5); idx <- c(3,3,1,3,3,3); n <- 30
  val <- as.numeric(mat)
  htable <- hash::hash(keys = as.character(val), values = 1:length(val))
  res <- .form_lp_obj(mat, idx, htable, n)

  vec <- c(rep(0,6), rep(-1,6), rep(1, 6), rep(0, 12))
  vec[c(9,15)] <- 0

  expect_true(all(res == vec))
})

###########################

## .form_lp_mpm is correct

test_that(".form_lp_mpm works", {
  mat <- matrix(1:30,6,5); idx <- rep(3,6); n <- 30
  val <- as.numeric(mat)
  htable <- hash::hash(keys = as.character(val), values = 1:length(val))

  res <- .form_lp_mpm(mat, idx, htable, n)

  expect_true(all(dim(res$constraint_mat) == c(6 + 29 + 1 + 1, 30)))
  expect_true(length(res$constraint_vec) == nrow(res$constraint_mat))
  expect_true(length(res$obj_vec) == 30)
})

test_that(".form_lp_mpm returns the right values for a base case", {
  mat <- matrix(1:30,6,5); idx <- rep(3,6); n <- 30
  val <- as.numeric(mat)
  htable <- hash::hash(keys = as.character(val), values = 1:length(val))

  res <- .form_lp_mpm(mat, idx, htable, n)

  expect_true(all(res$constraint_vec == c(rep(0.2,6), rep(0, 29), 1, 0)))
  for(i in 1:6){
    vec <- rep(0, n); vec[12+i] <- -1; vec[24+i] <- 1
    expect_true(all(res$constraint_mat[i,] == vec))
  }
  for(i in 7:(6+29)){
    vec <- rep(0, n); vec[c(i-6,i-5)] <- c(-1,1)
    expect_true(all(res$constraint_mat[i,] == vec))
  }
  expect_true(all(res$constraint_mat[6+29+1,] == c(rep(0, 29), 1)))
  expect_true(all(res$constraint_mat[6+29+2,] == c(-1,rep(0, 29))))
  expect_true(all(res$obj_vec == c(rep(0,6), rep(-1,6), rep(1, 6), rep(0, 12))))
})

##################################

## .estimate_mpm is correct

test_that(".estimate_mpm works", {
  mat <- matrix(1:30,6,5); idx <- rep(3,6)
  res <- .estimate_mpm(mat, idx)

  expect_true(class(res) == "mpm")
  expect_true(length(res$breakpoints) == length(res$values))
  expect_true(all(res$breakpoints == sort(res$breakpoints)))
  expect_true(all(res$values >= 0))
  expect_true(all(res$values <= 1))
})


