context("Test synthetic argument grabber")

## .snippet is correct

test_that(".snippet works", {
  res <- .snippet("(0,1)")
  expect_true(all(res == c(0,1)))
})

test_that(".snippet works when there is random characters afterwards", {
  res <- .snippet("(10, 15) blah blah blah")
  expect_true(all(res == c(10, 15)))
})

test_that(".snippet works with random spaces", {
  res <- .snippet("(  10  , 15  ) blah blah blah")
  expect_true(all(res == c(10, 15)))
})

test_that(".snippet works with more parentheses behind", {
  res <- .snippet("(  10  , 15  ) blah (5, 1)")
  expect_true(all(res == c(10, 15)))
})

######################

## .synthetic_arg_grabber is correct

test_that(".synthetic_arg_grabber works", {
  func <- function(dat, param1 = c(1,2), param2 = c(0,1), ...){
    dat
  }

  res <- .synthetic_arg_grabber(func)

  expect_true(is.list(res))
  expect_true(length(res) == 2)
  expect_true(all(res[[1]] == c(1,2)))
  expect_true(all(res[[2]] == c(0,1)))
})
