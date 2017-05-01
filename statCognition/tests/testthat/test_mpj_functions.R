context("Test the functions for monotone piecewise joint")

## .add_mpj is correct

test_that(".add_mpj works", {
  lis <- vector("list", 2)
  lis[[1]] <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  lis[[2]] <- .monotone_piecewise_marginal(c(2,4,6,8), c(10,11,12,13))

  obj1 <- .mpj_from_mpm(lis); obj2 <- .mpj_from_mpm(lis)
  res <- .add_mpj(obj1, obj2)

  expect_true(class(res) == "mpj")
  expect_true(all(res[[1]]$breakpoints == c(1,3,5,7)))
  expect_true(all(res[[1]]$values == c(20,22,24,26)))
  expect_true(all(res[[2]]$breakpoints == c(2,4,6,8)))
  expect_true(all(res[[2]]$values == c(20,22,24,26)))
})
