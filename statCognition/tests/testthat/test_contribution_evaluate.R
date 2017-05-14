context("Test contribution evaluate")

## evaluate.contribution is correct

test_that("evaluate.contribution works", {
  obj <- contribution(c(1,3,5,7), c(10,11,12,13))
  res <- evaluate(obj, 2)

  expect_true(res == 10)
})

test_that("evaluate.contribution works when a breakpoint is evaluated", {
  obj <- contribution(c(1,3,5,7), c(10,11,12,13))
  res <- evaluate(obj, 3)

  expect_true(res == 11)
})

test_that("evaluate.contribution works when a point before all breakpoints evaluated", {
  obj <- contribution(c(1,3,5,7), c(10,11,12,13))
  res <- evaluate(obj, 0)

  expect_true(res == 10)
})

test_that("evaluate.contribution works when a point after all breakpoints evaluated", {
  obj <- contribution(c(1,3,5,7), c(10,11,12,13))
  res <- evaluate(obj, 10)

  expect_true(res == 13)
})
