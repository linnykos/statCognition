context("Test difference states")

## difference_states_indicator is correct

test_that("difference_states_indicator works", {
  expect_true(difference_states_indicator(1:5, 1:5) == 0)
})

test_that("difference_states_indicator can return 0", {
  expect_true(difference_states_indicator(1:5, 6:10) == 1)
})
