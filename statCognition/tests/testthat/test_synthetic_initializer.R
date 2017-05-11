context("Test synthetic initializer")

## synthetic_initializer is correct

test_that("synthetic_initializer works", {
  res <- synthetic_initializer()

  expect_true(is.list(res))
  expect_true(class(res) == "synthetic_initializer")
  expect_true(length(res) >= 1)
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
