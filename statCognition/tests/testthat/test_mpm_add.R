context("Test adding mpm")

## .order_vectors is correct

test_that(".order_vectors works", {
  obj1 <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  obj2 <- .monotone_piecewise_marginal(c(2,4,6,8), c(10,11,12,13))

  res <- .order_vectors(obj1, obj2)

  expect_true(length(res) == 2)
  expect_true(all(res$vec1 == c(10, 0, 11, 0, 12, 0, 13, 0)))
  expect_true(all(res$vec2 == c(0, 10, 0, 11, 0, 12, 0, 13)))
})

###############################

## .fill_zeros is correct

test_that(".fill_zeros works", {
  vec <- c(0, 0, 1, 0, 0, 5, 6, 7, 0, 9, 0)
  res <- .fill_zeros(vec)

  expect_true(all(res == c(0, 0, 1, 1, 1, 5, 6, 7, 7, 9, 9)))
})

test_that(".fill_zeros works if the first element is non-zero", {
  vec <- c(10, 0, 11, 0, 12, 0, 13, 0)
  res <- .fill_zeros(vec)

  expect_true(all(res == c(10, 10, 11, 11, 12, 12, 13, 13)))
})

##############################

## .add_mpm is correct

test_that(".add_mpm works", {
  obj1 <- .monotone_piecewise_marginal(c(1,3,5,7), c(10,11,12,13))
  obj2 <- .monotone_piecewise_marginal(c(2,4,6,8), c(10,11,12,13))

  res <- .add_mpm(obj1, obj2)

  expect_true(class(res) == "mpm")
  expect_true(all(res$breakpoints == 1:8))
  expect_true(all(res$values == c(10,20:26)))
})
