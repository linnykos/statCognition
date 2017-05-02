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

#####################

## .estimate_mpj is correct

test_that(".estimate_mpj works", {
  mat_lis <- list(matrix(1:10,2,5), matrix(1:10,2,5))
  idx_lis <- list(rep(3,2), rep(3,2))

  res <- .estimate_mpj(mat_lis, idx_lis)

  expect_true(class(res) == "mpj")
  for(i in 1:2){
    expect_true(length(res[[i]]$breakpoints) == length(res[[i]]$values))
    expect_true(all(res[[i]]$breakpoints == sort(res[[i]]$breakpoints)))
    expect_true(all(res[[i]]$values >= 0))
    expect_true(all(res[[i]]$values <= 1))
  }
})

#####################

## .evaluate_mpj is correct

test_that(".evaluate_mpj works", {
  lis <- vector("list", 2)
  lis[[1]] <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  lis[[2]] <- .monotone_piecewise_marginal(c(2,4,6,8), c(10,11,12,13))

  obj <- .mpj_from_mpm(lis)
  res <- .evaluate_mpj(obj, c(3,8))

  expect_true(all(res == c(11,13)))
})
