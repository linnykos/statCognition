context("Test mixing helpers")

## mixing_shuffle_columns is correct

test_that("mixing_shuffle_columns works", {
  mat <- matrix(1:30, 5, 6)
  mat2 <- mixing_shuffle_columns(mat, percentage_columns = 1)

  expect_true(all(dim(mat2) == dim(mat)))
})
