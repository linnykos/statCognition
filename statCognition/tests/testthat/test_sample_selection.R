context("Test sample selection")

## SS_none is correct

test_that("SS_none works", {
  mat <- matrix(1:30, 5, 6)
  res <- SS_none(mat)

  expect_true(is.list(res))
})

#####################

## SS_three_sd is correct

test_that("SS_three_sd works", {
  mat <- matrix(1:30, 5, 6)
  res <- SS_three_sd(mat)

  expect_true(is.list(res))
})

#####################

## SS_quantile is correct

test_that("SS_quantile works", {
  mat <- matrix(1:30, 5, 6)
  res <- SS_quantile(mat)

  expect_true(is.list(res))
})

#####################

## SS_neighborhood is correct

test_that("SS_neighborhood works", {
  mat <- matrix(1:30, 5, 6)
  res <- SS_neighborhood(mat)

  expect_true(is.list(res))
})


