context("Test mixing helpers")

## mixing_shuffle_columns is correct

test_that("mixing_shuffle_columns works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_shuffle_columns(mat, percentage_columns = 1)

  expect_true(all(dim(mat2) == dim(mat)))
})


## mixing_shuffle_rows is correct

test_that("mixing_shuffle_rows works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_shuffle_rows(mat, percentage_columns = 1)

  expect_true(all(dim(mat2) == dim(mat)))
})

## mixing_refit_normality is correct

test_that("mixing_refit_normality works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_refit_normality(mat)

  expect_true(all(dim(mat2) == dim(mat)))
})

## mixing_resample is correct

test_that("mixing_resample works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_resample(mat)

  expect_true(all(dim(mat2) == dim(mat)))
})

## mixing_swap_rows is correct

test_that("mixing_swap_rows works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_swap_rows(mat, 1:2, 3:4)

  expect_true(all(dim(mat2) == dim(mat)))
})

## mixing_adjust_mean is correct

test_that("mixing_adjust_mean works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_adjust_mean(mat)

  expect_true(all(dim(mat2) == dim(mat)))
})

## mixing_inflate_correlation is correct

test_that("mixing_inflate_correlation works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_inflate_correlation(mat)

  expect_true(all(dim(mat2) == dim(mat)))
})
