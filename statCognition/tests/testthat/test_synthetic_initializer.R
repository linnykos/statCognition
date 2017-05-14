context("Test synthetic initializer")

## synthetic_initializer is correct

test_that("synthetic_initializer works", {
  res <- synthetic_initializer()

  expect_true(is.list(res))
  expect_true(class(res) == "synthetic_initializer")
  expect_true(length(res) >= 1)

  expect_true(is.numeric(attr(res, "lambda")))
})

############################

## .grab_package_contents is correct

test_that(".grab_package_contents works", {
  res <- .grab_package_contents("lm", package_name = "stats")

  expect_true(is.list(res))
  expect_true("lm" %in% names(res))

  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  idx <- which(names(res) == "lm")
  lm.D9 <- res[[idx]](weight ~ group)
  expect_true(class(lm.D9) == "lm")
})

########################

## .generate_parameter_values is correct

test_that(".generate_parameter_values works", {
  lis <- .synthetic_arg_grabber(generator_add_noise)
  res <- .generate_parameter_values(lis)

  expect_true(length(res) == length(lis))
  expect_true(all(is.numeric(res)))
})

test_that(".generate_parameter_values works with min", {
  lis <- .synthetic_arg_grabber(generator_add_noise)
  res <- .generate_parameter_values(lis, min)
  res2 <- .generate_parameter_values(lis)

  expect_true(all(res <= res2))
})

test_that(".generate_parameter_values works with max", {
  lis <- .synthetic_arg_grabber(generator_add_noise)
  res <- .generate_parameter_values(lis, max)
  res2 <- .generate_parameter_values(lis)

  expect_true(all(res >= res2))
})

######################

## .apply_generator2dat is correct

test_that(".apply_generator2dat works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  lis <- .synthetic_arg_grabber(generator_add_noise)
  vec <- .generate_parameter_values(lis, min)

  res <- .apply_generator2dat(dat, generator_add_noise, vec)
  expect_true(all(dim(res) == c(6,5)))
})

test_that(".apply_generator2dat works with a generator w/ one parameter", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5)))
  lis <- .synthetic_arg_grabber(generator_resample)
  vec <- .generate_parameter_values(lis, min)

  res <- .apply_generator2dat(dat, generator_resample, vec)
  expect_true(all(dim(res) == c(6,5)))
})

######################

## is_valid.synthetic_initializer is correct

test_that("is_valid.synthetic_initializer works", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()

  expect_true(is_valid(init, dat))
})

test_that("is_valid.synthetic_initializer errors if I screw it up dramatically", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()
  init[[1]] <- stats::lm

  expect_error(is_valid(init, dat))
})

test_that("is_valid.synthetic_initializer errors if I screw it up subtly", {
  set.seed(10)
  dat <- data_object(list(mat = matrix(1:60, 6, 5), pheno = data.frame(age = 1:6)))
  init <- synthetic_initializer()
  init[[1]] <- function(dat, param1 = c(0, 1), ...){
    invisible()
  }

  expect_error(is_valid(init, dat))
})
