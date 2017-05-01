context("Test constructing monotone piecewise marginals")

## .monotone_piecewise_marginal is correct

test_that(".monotone_piecewise_marginal works", {
  res <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))

  expect_true(class(res) == "mpm")
  expect_true(all(res$breakpoints == c(1,3,5,7)))
  expect_true(all(res$values == 10:13))
})

###############################

## .mpj_from_mpm is correct

test_that(".mpj_from_mpm works", {
  lis <- vector("list", 2)
  lis[[1]] <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  lis[[2]] <- .monotone_piecewise_marginal(c(2,4,6,8), c(10,11,12,13))

  res <- .mpj_from_mpm(lis)

  expect_true(length(res) == 2)
  expect_true(class(res) == "mpj")
  expect_true(unique(sapply(res, class)) == "mpm")
})

#########################

## .monotone_piecewise_joint is correct

test_that(".monotone_piecewise_joint works", {
  breakpoint_lis <- list(c(1,3,5,7), c(2,4,6,8))
  value_lis <- list(c(10,11,12,13), c(10,11,12,13))

  res <- .monotone_piecewise_joint(breakpoint_lis, value_lis)

  expect_true(length(res) == 2)
  expect_true(class(res) == "mpj")
  expect_true(unique(sapply(res, class)) == "mpm")
})

##################

## .find_breakpoints is correct

test_that(".find_breakpoints finds the correct breakpoints", {
  vec <- c(1,1,1,2,2,2,2,3,3,3,5,5,5,6,7,9,9,10)
  res <- .find_breakpoints(vec)

  expect_true(all(res == c(4,8,11,14,15,16,18)))
})

################

## .remove_duplicates is correct

test_that(".remove_duplicates removes the correct points", {
  breakpoints <- c(0,1,1,1,2,2,2,2,3,3,3,5,5,5,6,7,9,9,10)
values <- 0:18

  res <- .remove_duplicates(breakpoints, values)

  expect_true(all(res$breakpoints == c(0,1,2,3,5,6,7,9,10)))
  expect_true(all(res$values == c(0,3,7,10,13,14,15,17,18)))
})
