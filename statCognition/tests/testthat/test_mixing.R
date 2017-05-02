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

######################

## .generate_synthetic_data_from_seed is correct

test_that(".generate_synthetic_data_from_seed works", {
  mat <- matrix(1:30, 5, 6)
  init <- generate_synthetic_initializer()
  mat2 <- .generate_synthetic_data_from_seed(mat, init)

  expect_true(is.matrix(mat2))
})

##########################

## generate_synthetic_initializer is correct

test_that("generate_synthetic_initializer works", {
  res <- generate_synthetic_initializer()
  expect_true(class(res) == "generator_init")
})
