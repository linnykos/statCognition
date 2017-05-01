context("Test constructing monotone piecewise marginals")

## .monotone_piecewise_marginal is correct

test_that(".monotone_piecewise_marginal works", {
  res <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))

  expect_true(class(res) == "mpm")
  expect_true(all(res$breakpoints == c(1,3,5,7)))
  expect_true(all(res$values == 10:13))
})

###############################

## .monotone_piecewise_joint is correct

test_that(".monotone_piecewise_joint works", {
  breakpoint_lis <- list(c(1,3,5,7), c(2,4,6,8))
  value_lis <- list(c(10,11,12,13), c(10,11,12,13))

  res <- .monotone_piecewise_joint(breakpoint_lis, value_lis)

  expect_true(length(res) == 2)
  expect_true(class(res) == "mpj")
  expect_true(unique(sapply(res, class)) == "mpm")
})


