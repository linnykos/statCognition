context("Test synthetic generator functions: phenotype")

## generator_resample_pheno is correct

test_that("generator_resample_pheno works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_resample_pheno(dat, 0.5)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_resample_pheno works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_resample_pheno(dat, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_linearize_pheno is correct

test_that("generator_linearize_pheno works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_linearize_pheno(dat, 1, 1)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_linearize_pheno works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_linearize_pheno(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_monotonic_pheno is correct

test_that("generator_monotonic_pheno works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_monotonic_pheno(dat, 1, 0)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_monotonic_pheno works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_monotonic_pheno(dat, 1e-4, 1e-4)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

##################

## generator_cluster_pheno is correct

test_that("generator_cluster_pheno works", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_cluster_pheno(dat, 1, 1, 2)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_cluster_pheno works with 0", {
  set.seed(10)
  dat <- statCognition::dat
  res <- generator_cluster_pheno(dat, 1e-4, 1e-4, 3)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})

test_that("generator_cluster_pheno does not introduce new values", {
  set.seed(10)
  dat <- statCognition::dat

  set.seed(710+1)
  idx <- sample(1:15, 1)
  lis <- .synthetic_arg_grabber(generator_cluster_pheno)
  vec <- .generate_parameter_values(lis)
  dat <- .apply_generator2dat(dat, generator_cluster_pheno, vec)

  res <- generator_cluster_pheno(dat, 1e-4, 1e-4, 3)

  expect_true(is.matrix(res$mat))
  expect_true(class(res) == "data")
  expect_true(!any(is.na(res$mat)))
  expect_true(all(dim(res$mat) == dim(dat$mat)))
})


######################

## .select_continuous_idx is correct

test_that(".select_continuous_idx works", {
  set.seed(10)
  pheno <- data.frame(age = 1:100, sex = rep(c("M", "F"), 50),
                      type = as.factor(rep(c("A", "B", "C", "D"),25)),
                      intellec = stats::rnorm(100))

  vec <- sapply(1:100, function(x){.select_continuous_idx(pheno)})
  res <- sort(unique(vec))

  expect_true(all(res == c(1,4)))
})
