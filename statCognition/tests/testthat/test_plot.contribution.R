context("Test plotting contributions")

## plot.contribution is correct

test_that("plot.contribution works", {
  set.seed(10)
  loc <- 1:50
  val <- abs(rnorm(50) + rep(seq(0,40,length.out = 5), each = 10))
  val2 <- abs(rnorm(50) + rep(seq(0,40,length.out = 5), each = 10))

  obj <- .contribution_estimate(loc, val, val2, store = T)

  pdf("Rplots.pdf", height = 6, width = 8)
  plot(obj)
  graphics.off(); file.remove("Rplots.pdf")

  expect_true(TRUE)
})
