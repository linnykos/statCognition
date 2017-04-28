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

#################################

## .assign_values_to_mpm is correct

test_that(".assign_values_to_mpm works", {
  res <- .assign_values_to_mpm(c(5:9, 3))
  expect_true(all(res == c(0,0,2/4,2/4,2/4)))
})

##################################

## .remove_zero_values is correct

test_that(".remove_zero_values works", {
  mat <- matrix(1:10,2,5); idx <- c(2,4)
  val <- matrix(c(0, rep(1/4, 4), rep(0,3), rep(3/4, 2)), 2, 5, byrow = T)

  res <- .remove_zero_values(mat, idx, val)

  expect_true(length(res) == 2)
  expect_true(is.list(res))
  expect_true(all(sapply(res, is.numeric)))
})

test_that(".remove_zero_values returns the right values in a simple case", {
  mat <- matrix(1:10,2,5); idx <- c(2,4)
  val <- matrix(c(0, rep(1/4, 4), rep(0,3), rep(3/4, 2)), 2, 5, byrow = T)

  res <- .remove_zero_values(mat, idx, val)

  expect_true(all(res$x == c(1,2,3,5,7,8,9,10)))
  expect_true(all(res$y == c(0,0,1/4,1/4,1/4,3/4,1/4,3/4)))
})

##################################

## .estimate_mpm is correct

test_that(".estimate_mpm works", {
  mat <- matrix(1:10,2,5); idx <- rep(3,2)
  res <- .estimate_mpm(mat, idx)

  expect_true(class(res) == "mpm")
  expect_true(length(res$breakpoints) == length(res$values))
  expect_true(all(res$breakpoints == sort(res$breakpoints)))
  expect_true(all(res$values >= 0))
  expect_true(all(res$values <= 1))
})


