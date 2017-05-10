context("Test value contribution")

test_that(".value_contribution works", {
  set.seed(10)
  vec1 <- stats::rnorm(50); vec2 <- c(stats::rnorm(48),10,-10)
  dat <- data_object(list(mat = cbind(vec1, vec2)))

  action_list <- list(SS_none = SS_none, SS_cook = SS_cook)
  state_list <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  state_list_future <- list(state_samples = state_samples, state_linearity = state_linearity)
  outcome_state <- SS_cook(dat)

  #set up val
  contribution_l1 <- list(pearson = contribution(c(.2, .5, 9), c(0, 1, 1)),
                          kendall = contribution(c(.1, .3, .8), c(0, 0, 1)))
  contribution_l2 <- list(pearson = contribution(c(.4, .8, .9), c(0, 1, 1)),
                          kendall = contribution(c(.2, .5, .9), c(1, 1, 1)))
  contribution_ll <- list(contribution_l1, contribution_l2)
  val <- value_estimate(contribution_ll)

  #res <- .value_contribution(dat, action_list, state_list, state_list_future,
  #                           outcome_state, val)

  #expect_true(is.list(res))
})
