context("Test value function")

## value is correct

test_that("value works", {
  h <- hash::hash(c("1", "2", "3"), list(data.frame(value = 1, action = 1),
                                   data.frame(value = 2, action = 2),
                                   data.frame(value = 3, action = 3)))
  tab <- c(5, 10, 32)
  surface <- list(hash = h, block_list = tab)
  res <- value(surface)

  expect_true(class(res) == "value")
  expect_true(is_valid(res))
})
