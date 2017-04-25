context("Test criterion extracting")

## criterion_extract_data_default is correct

test_that("criterion_extract_data_default works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  res <- criterion_extract_data_default(mat, pheno)

  expect_true(is.numeric(res))
  expect_true(!is.matrix(res))
})
