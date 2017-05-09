context("Test state correlation")

## state_monotone is correct

test_that("state_monotone works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  dat <- data_object(list(mat = mat))

  res <- state_monotone(dat)

  expect_true(is.numeric(res))
})

###############

## state_samples is correct

test_that("state_samples works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  dat_prev <- data_object(list(mat = mat))
  dat <- SS_cook(dat_prev)

  res <- state_samples(dat, dat_prev)

  expect_true(is.numeric(res))
})

###################

## state_linearity is correct

test_that("state_linearity works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  dat <- data_object(list(mat = mat))

  res <- state_linearity(dat)

  expect_true(is.numeric(res))
})

#################

## .generate_pairs is correct

test_that(".generate_pairs works", {
  res <- .generate_pairs(50, 50)

  vec <- c(res[1,]+50*res[2,], 50*res[1,]+res[2,])
  expect_true(length(unique(vec)) == 100)
})

######################

## .alternating_direction_sum is correct

test_that(".alternating_direction_sum works", {
  vec <- 1:50
  res <- .alternating_direction_sum(vec)
  expect_true(res == 0)
})

test_that(".alternating_direction_sum is non-zero for alternating sums", {
  vec <- c(rep(1,10), rep(0, 10), rep(-1, 10), rep(2, 10), rep(1, 10), rep(3, 10))
  res <- .alternating_direction_sum(vec)
  expect_true(res == 3+1+2)
})

test_that(".alternating_direction_sum works when monotonically decreasing once", {
  vec <- c(rep(-1,10), rep(-6,10))
  .alternating_direction_sum(vec)
})

#####################

## .reorder_vector is correct

test_that(".reorder_vector works", {
  set.seed(10)
  x <- sample(1:10)
  y <- rnorm(10)
  res <- .reorder_vector(x, y)

  expect_true(all(res$vec1 == sort(x, decreasing = F)))
  for(i in 1:10){
    expect_true(res$vec2[which(res$vec1 == i)] == y[which(x == i)])
  }
})
