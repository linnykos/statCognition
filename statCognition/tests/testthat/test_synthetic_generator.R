context("Test synthetic generator")

## .synthetic_generator_seed is correct

test_that(".synthetic_generator_seed works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()

  res <- .synthetic_generator_seed(dat, init)

  expect_true(class(res) == "data")
  expect_true(all(dim(res$mat) == c(6,5)))
})

test_that(".synthetic_generator_seed is reproducible", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()

  res <- .synthetic_generator_seed(dat, init, seed = 5)
  res2 <- .synthetic_generator_seed(dat, init, seed = 5)

  expect_true(all(res$mat == res2$mat))
})


##################

## synthetic_generator is correct

test_that("synthetic_generator works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()

  res <- synthetic_generator(dat, init)

  expect_true(class(res) == "data")
  expect_true(all(dim(res$mat) == c(6,5)))
  expect_true("synthetic_seed" %in% names(res))
})
