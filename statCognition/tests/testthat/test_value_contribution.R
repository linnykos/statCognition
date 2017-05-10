context("Test value contribution")

## .store_state_action is correct

test_that(".store_state_action works", {
  set.seed(10)
  vec1 <- stats::rnorm(50); vec2 <- c(stats::rnorm(48),10,-10)
  dat <- data_object(list(mat = cbind(vec1, vec2)))

  action_list <- list(SS_none = SS_none, SS_cook = SS_cook)
  state_list <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  state_list_future <- list(state_samples = state_samples, state_linearity = state_linearity)
  outcome_state <- SS_cook(dat)

  res <- .store_state_action(dat, action_list, state_list, state_list_future,
                             prev_dat = dat, response = 1)

  expect_true(is.list(res))
  expect_true(class(res) == "state_action")
  expect_true(all(names(res) == c("current", "future", "response")))
})


test_that(".store_state_action works without future states", {
  set.seed(10)
  vec1 <- stats::rnorm(50); vec2 <- c(stats::rnorm(48),10,-10)
  dat <- data_object(list(mat = cbind(vec1, vec2)))

  action_list <- list(SS_none = SS_none, SS_cook = SS_cook)
  state_list <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  outcome_state <- SS_cook(dat)

  res <- .store_state_action(dat, action_list, state_list, NA,
                             prev_dat = dat, response = 1)

  expect_true(is.list(res))
  expect_true(class(res) == "state_action")
  expect_true(all(names(res) == c("current", "future", "response")))
})


########################

## is_valid.state_action is correct

test_that("is_valid.state_action fails when I tamper with the object", {
  set.seed(10)
  vec1 <- stats::rnorm(50); vec2 <- c(stats::rnorm(48),10,-10)
  dat <- data_object(list(mat = cbind(vec1, vec2)))

  action_list <- list(SS_none = SS_none, SS_cook = SS_cook)
  state_list <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  state_list_future <- list(state_samples = state_samples, state_linearity = state_linearity)
  outcome_state <- SS_cook(dat)

  res <- .store_state_action(dat, action_list, state_list, state_list_future,
                             prev_dat = dat, response = 1)

  res$future[[1]] <- 1:5

  expect_error(is_valid(res))
})

#######################

## .state_extract is correct

test_that(".state_extract works", {
  set.seed(10)
  vec1 <- stats::rnorm(50); vec2 <- c(stats::rnorm(48),10,-10)
  dat <- data_object(list(mat = cbind(vec1, vec2)))
  state_list <- list(state_variance = state_variance, state_interpoint = state_interpoint)

  res <- .state_extract(dat, state_list)

  expect_true(length(res) == 2)
  expect_true(is.numeric(res))
})

########################

## .value_contribution is correct

test_that(".value_contribution works", {
  set.seed(10)
  vec1 <- stats::rnorm(50); vec2 <- c(stats::rnorm(48),10,-10)
  dat <- data_object(list(mat = cbind(vec1, vec2)))

  action_list <- list(SS_none = SS_none, SS_cook = SS_cook)
  state_list <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  state_list_future <- list(state_samples = state_samples, state_linearity = state_linearity)
  outcome_state <- SS_cook(dat)

  state_action <- .store_state_action(dat, action_list, state_list, state_list_future,
                             prev_dat = dat, response = 1)

  #set up val
  contribution_l1 <- list(pearson = contribution(c(.2, .5, 9), c(0, 1, 1)),
                          kendall = contribution(c(.1, .3, .8), c(0, 0, 1)))
  contribution_l2 <- list(pearson = contribution(c(.4, .8, .9), c(0, 1, 1)),
                          kendall = contribution(c(.2, .5, .9), c(1, 1, 1)))
  contribution_ll <- list(contribution_l1, contribution_l2)
  val <- value_estimate(contribution_ll)

  res <- .value_contribution(state_action, val)

  expect_true(is.list(res))
  expect_true(length(res) == 2)
  expect_true(all(lapply(res, length) == c(2,2)))
})


