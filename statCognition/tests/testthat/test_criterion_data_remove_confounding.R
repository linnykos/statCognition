context("Test criterion data remove confounders")

## criterionData_pheno_residual_RF_LR is correct

test_that("criterionData_pheno_residual_RF_LR works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  res <- criterionData_pheno_residual_RF_LR(mat, pheno)

  expect_true(is.numeric(res))
})

#######################

## criterionData_pheno_MI is correct

test_that("criterionData_pheno_MI works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- rep(1:5, each=10)
  gender <- as.factor(rep(c("M", "F", "M", "M", "F"), each = 10))
  pheno <- data.frame(age, gender)

  res <- criterionData_pheno_MI(mat, pheno)

  expect_true(is.numeric(res))
})
