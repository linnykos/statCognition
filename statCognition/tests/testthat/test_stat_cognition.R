context("Test stat cognition system")

## stat_cognition is correct

test_that("stat_cognition works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- 1:50
  gender <- as.factor(c(rep("M", 25), rep("F", 25)))
  pheno <- data.frame(age, gender)

  action_list_list <- list(sample_selection =
                             list(SS_none = statCognition::SS_none,
                                  SS_three_sd = statCognition::SS_three_sd),
                           remove_confounding =
                             list(RC_none = statCognition::RC_none,
                                  RC_linear_regression = statCognition::RC_linear_regression))
  state_list <- list(influential_points = statCognition::state_data_influential_points,
                     nearest_neighbor = statCognition::state_data_nearest_neighbor)

  init <- stat_cognition_initializer(action_list_list, state_list)

  seed_vec <- c(1,2)
  response_vec <- rep(1, 2*2)

  res <- stat_cognition(mat, pheno, init, seed_vec, response_vec)

  expect_true(is.list(res))
  expect_true(class(res) == "mpj_sequence")
  expect_true(length(res) == 2)
  expect_true(all(sapply(res, class) == "mpj"))
})

#################################

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
                                     num_mat = 4, num_act_vec = rep(5, 2))

  expect_true(length(res) == 2)
  expect_true(is.list(res))
  expect_true(length(res$value_list_list) == 2)
  expect_true(length(res$value_list_list[[1]]) == 3)
  expect_true(all(dim(res$value_list_list[[1]][[1]]) == c(4, 5)))

  expect_true(is.list(res$idx_list))
  expect_true(length(res$idx_list[[1]]) == 4)
})

#############################

## .apply_action_return_state is correct

test_that(".apply_action_return_state works", {
  set.seed(10)
  mat <- matrix(rnorm(30), 5, 6)
  age <- 1:5
  gender <- as.factor(c("M", "F", "M", "M", "F"))
  pheno <- data.frame(age, gender)

  state_list <- list(influential_points = statCognition::state_data_influential_points,
                     nearest_neighbor = statCognition::state_data_nearest_neighbor)

  res <- .apply_action_return_state(mat, pheno, statCognition::RC_linear_regression,
                                    state_list)

  expect_true(is.numeric(res))
  expect_true(length(res) == length(state_list))
})

##############################

## .apply_all_actions_return_states is correct

test_that(".apply_all_actions_return_states works", {
  set.seed(10)
  mat <- matrix(rnorm(300), 50, 6)
  age <- 1:50
  gender <- as.factor(c(rep("M", 25), rep("F", 25)))
  pheno <- data.frame(age, gender)

  state_list <- list(influential_points = statCognition::state_data_influential_points,
                     nearest_neighbor = statCognition::state_data_nearest_neighbor,
                     pheno_residual = statCognition::state_data_pheno_MI)
  action_list <- list(RC_none = statCognition::RC_none,
                      RC_linear_regression = statCognition::RC_linear_regression)

  res <- .apply_all_actions_return_states(mat, pheno, action_list, state_list)

  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(length(state_list), length(action_list))))
})
