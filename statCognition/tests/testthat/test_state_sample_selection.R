context("Test state sample selection")

## state_variance is correct

test_that("state_variance works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  dat <- data_object(list(mat = mat))

  res <- state_variance(dat)

  expect_true(is.numeric(res))
})

#######################

## state_interpoint is correct

test_that("state_interpoint works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  dat <- data_object(list(mat = mat))

  res <- state_interpoint(dat)

  expect_true(is.numeric(res))
})
