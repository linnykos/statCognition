context("Test statistical cognition")

## .estimate_value_cognition is correct

test_that(".estimate_value_cognition works", {
  #construct the 2 datasets
  set.seed(10)
  dat1 <- data_object(list(mat = matrix(stats::rnorm(100), 50, 2)))
  x <- stats::rnorm(48); y <- x^3 - 2*x + 0.2*stats::rnorm(48)
  x <- c(x, -1, 1); y <- c(y, -4, 1)
  dat2 <- data_object(list(mat = cbind(x,y)))

  #set up actions
  action_ll <- vector("list", 2)
  action_ll[[1]] <- list(SS_none = SS_none, SS_cook = SS_cook)
  action_ll[[2]] <- list(PD_pearson = PD_pearson, PD_energy = PD_energy)

  #set up states
  state_ll <- vector("list", 2)
  state_ll[[1]] <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  state_ll[[2]] <- list(state_samples = state_samples, state_linearity = state_linearity)

  #set up state_action
  state_action_ll <- vector("list", 2)
  state_action_ll[[1]][[1]] <- .store_state_action(dat1, action_ll[[1]], state_ll[[1]],
                                                   state_ll[[2]], dat1, 1)
  dat1b <- SS_none(dat1)
  state_action_ll[[1]][[2]] <- .store_state_action(dat1b, action_ll[[2]], state_ll[[2]],
                                                   NA, dat1, 1)
  state_action_ll[[2]][[1]] <- .store_state_action(dat2, action_ll[[1]], state_ll[[1]],
                                                   state_ll[[2]], dat2, 2)
  dat2b <- SS_cook(dat2)
  state_action_ll[[2]][[2]] <- .store_state_action(dat2b, action_ll[[2]], state_ll[[2]],
                                                   NA, dat2, 2)

  init <- stat_cognition_initializer(action_ll = action_ll, state_ll <- state_ll)

  res <- .estimate_value_cognition(state_action_ll, init)

  expect_true(class(res) == "list")
  expect_true(all(sapply(res, class) == "value"))
  expect_true(all(sapply(res, is_valid)))
})

############################

## stat_cognition is correct

test_that("stat_cognition works", {
  #construct data
  set.seed(10)
  dat <- data_object(list(mat = matrix(stats::rnorm(100), 50, 2),
                          pheno = data.frame(age = 1:50)))

  #set up actions
  action_ll <- vector("list", 2)
  action_ll[[1]] <- list(SS_none = SS_none, SS_cook = SS_cook)
  action_ll[[2]] <- list(PD_pearson = PD_pearson, PD_energy = PD_energy)

  #set up states
  state_ll <- vector("list", 2)
  state_ll[[1]] <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  state_ll[[2]] <- list(state_samples = state_samples, state_linearity = state_linearity)

  seed_vec <- c(10, 50)
  response_vec <- c(1,2,2,1)
  init <- stat_cognition_initializer(action_ll, state_ll)

  res <- stat_cognition(dat, init, seed_vec, response_vec)

  expect_true(is.list(res))
  expect_true(length(res) == 2)
  expect_true(all(sapply(res, class) == "value"))
})
