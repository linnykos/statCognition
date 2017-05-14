context("Test value evaluate")

## evaluate.value is correct

test_that("evaluate.value returns value", {
  contribution_l1 <- list(action1 = contribution(seq(1,10,3), c(10,1:3)+.5),
                          action2 = contribution(seq(3,15,5), 6:8+.5))
  contribution_l2 <- list(action1 = contribution(seq(11,20,3), c(10,1:3)+.1),
                          action2 = contribution(seq(13,25,5), 6:8+.1))
  contribution_ll <- list(contribution_l1, contribution_l2)

  obj <- value_estimate(contribution_ll)
  res <- evaluate(obj, c(5,17))

  expect_true(is.numeric(res))
})

test_that("evaluate.value returns action", {
  contribution_l1 <- list(action1 = contribution(seq(1,10,3), c(10,1:3)+.5),
                          action2 = contribution(seq(3,15,5), 6:8+.5))
  contribution_l2 <- list(action1 = contribution(seq(11,20,3), c(10,1:3)+.1),
                          action2 = contribution(seq(13,25,5), 6:8+.1))
  contribution_ll <- list(contribution_l1, contribution_l2)

  obj <- value_estimate(contribution_ll)
  res <- evaluate(obj, c(5,17), return.value = F)

  expect_true(is.numeric(res))
  expect_true(res %% 1 == 0)
  expect_true(res >= 1)
  expect_true(res <= 2)
})

