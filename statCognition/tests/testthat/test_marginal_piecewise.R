context("Test constructing monotone piecewise marginals")

## .monotone_piecewise_marginal is correct

test_that(".monotone_piecewise_marginal works", {
  res <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))

  expect_true(class(res) == "mpm")
  expect_true(all(res$breakpoints == c(1,3,5,7)))
  expect_true(all(res$values == 10:13))
})
