context("Test stat cognition system")

## stat_cognition_initializer is correct

test_that("stat_cognition_initializer works", {
  action_list_list <- list(sample_selection =
                             list(SS_none = statCognition::SS_none,
                                SS_three_sd = statCognition::SS_three_sd),
                           remove_confounding =
                             list(RC_none = statCognition::RC_none,
                                  RC_linear_regression = statCognition::RC_linear_regression))
  state_list <- list(influential_points = statCognition::state_data_influential_points,
                     nearest_neighbor = statCognition::state_data_nearest_neighbor)

  res <- stat_cognition_initializer(action_list_list, state_list)

  expect_true(class(res) == "stat_cognition_initializer")
})

###############################

## .initialize_cognition_setup is correct

test_that(".initialize_cognition_setup works", {
  res <- .initialize_cognition_setup(num_step = 2, num_state = 3,
                                     num_dat = 4, num_act_vec = rep(5, 2))

  expect_true(length(res) == 2)
  expect_true(is.list(res))
  expect_true(length(res$mat_lis_lis) == 2)
  expect_true(length(res$mat_lis_lis[[1]]) == 3)
  expect_true(all(dim(res$mat_lis_lis[[1]][[1]]) == c(4, 5)))

  expect_true(is.list(res$idx_lis))
  expect_true(length(res$idx_lis[[1]]) == 4)
})
