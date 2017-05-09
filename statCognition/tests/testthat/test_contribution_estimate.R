context("Test contribution estimate")

## .contribution_estimate is correct

test_that(".contribution_estimate works", {
  set.seed(10)
  loc <- sample(1:50)
  val <- rnorm(50)

  res <- .contribution_estimate(loc, val)

  expect_true(class(res) == "contribution")
  expect_true(all(res$values <= max(val)))
  expect_true(all(res$values >= min(val)))
})
