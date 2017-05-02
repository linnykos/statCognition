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
