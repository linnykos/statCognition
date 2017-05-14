context("Test contribution object")

## contribution is correct

test_that("contribution works", {
  res <- contribution(c(1,3,5,7), c(10,11,12,13))

  expect_true(class(res) == "contribution")
  expect_true(all(res$breakpoints == c(1,3,5,7)))
  expect_true(all(res$values == 10:13))
})

test_that("contribution might have only one value and that's okay", {
  res <- contribution(c(1,3,5,7), rep(0,4))

  expect_true(res$breakpoints == 1)
  expect_true(res$values == 0)
})

##########

## .find_breakpoints is correct

test_that(".find_breakpoints finds the correct breakpoints", {
  vec <- c(1,1,1,2,2,2,2,3,3,3,5,5,5,6,7,9,9,10)
  res <- .find_breakpoints(vec)

  expect_true(all(res == c(4,8,11,14,15,16,18)))
})

################

## .remove_duplicate_breakpoints is correct

test_that(".remove_duplicate_breakpoints removes the correct points", {
  breakpoints <- c(0,1,1,1,2,2,2,2,3,3,3,5,5,5,6,7,9,9,10)
  values <- 0:18

  res <- .remove_duplicate_breakpoints(breakpoints, values)

  expect_true(all(res$breakpoints == c(0,1,2,3,5,6,7,9,10)))
  expect_true(all(res$values == c(0,3,7,10,13,14,15,17,18)))
})

#################

## .remove_duplicate_values is correct

test_that(".remove_duplicate_values removes the correct points", {
  breakpoints <- 0:18
  values <- c(0,1,1,1,2,2,2,2,3,3,3,5,5,5,6,7,9,9,10)

  res <- .remove_duplicate_values(breakpoints, values)

  expect_true(all(res$values == c(0,1,2,3,5,6,7,9,10)))
  expect_true(all(res$breakpoints == c(0,1,4,8,11,14,15,16,18)))
})
