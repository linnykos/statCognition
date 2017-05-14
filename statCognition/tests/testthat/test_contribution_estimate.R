context("Test contribution estimate")

## .contribution_estimate is correct

test_that(".contribution_estimate works", {
  set.seed(10)
  loc <- sample(1:50)
  val <- rnorm(50)

  res <- .contribution_estimate(loc, val)

  expect_true(class(res) == "contribution")
  expect_true(all(res$values <= max(val)))
  expect_true(all(res$values >= min(val)))
})

test_that(".contribution_estimate works with too few samples", {
  set.seed(10)
  loc <- sample(1:5)
  val <- rnorm(5)

  res <- .contribution_estimate(loc, val)

  expect_true(class(res) == "contribution")
  expect_true(all(res$values <= max(val)))
  expect_true(all(res$values >= min(val)))
})

######################

## .isoreg_try is corret

test_that(".isoreg_try works for negative", {
  x <- 1:10
  y <- -1:-10
  res <- .isoreg_try(x,y)

  expect_true(all(diff(res) < 0))
})

test_that(".isoreg_try works for positive", {
  x <- 1:10
  y <- 1:10
  res <- .isoreg_try(x,y)

  expect_true(all(diff(res) > 0))
})
