context("Test mixing")

## generate_mixing_default is correct

test_that("generate_mixing_default works", {
  res <- generate_mixing_default()

  expect_true(length(res) > 0)
  expect_true(is.list(res))

  mat <- matrix(1:30, 5, 6)
  for(i in 1:length(res)){
    mat2 <- res[[i]](mat)
    expect_true(all(dim(mat2) == dim(mat)))
  }
})
