context("Test evaluating monotone piecewise marginals")

## .evaluate_mpm is correct

test_that(".evaluate_mpm works", {
  obj <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  res <- .evaluate_mpm(obj, 2)

  expect_true(res == 10)
})

test_that(".evaluate_mpm works when a breakpoint is evaluated", {
  obj <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  res <- .evaluate_mpm(obj, 3)

  expect_true(res == 11)
})

test_that(".evaluate_mpm works when a point before all breakpoints evaluated", {
  obj <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  res <- .evaluate_mpm(obj, 0)

  expect_true(res == 10)
})

test_that(".evaluate_mpm works when a point after all breakpoints evaluated", {
  obj <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  res <- .evaluate_mpm(obj, 10)

  expect_true(res == 13)
})
