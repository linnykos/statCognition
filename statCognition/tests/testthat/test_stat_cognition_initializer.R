context("Test statistical cognition initializer")

## stat_cognition_initializer is correct

test_that("stat_cognition_initializer can be initialized", {
  action_ll <- vector("list", 2)
  action_ll[[1]] <- list(SS_none = SS_none, SS_cook = SS_cook)
  action_ll[[2]] <- list(PD_pearson = PD_pearson, PD_energy = PD_energy)

  #set up states
  state_ll <- vector("list", 2)
  state_ll[[1]] <- list(state_variance = state_variance, state_interpoint = state_interpoint)
  state_ll[[2]] <- list(state_samples = state_samples, state_linearity = state_linearity)

  res <- stat_cognition_initializer(action_ll = action_ll, state_ll = state_ll)

  expect_true(class(res) == "stat_cognition_initializer")
  expect_true(all(names(res) == c("action_ll", "state_ll", "difference_list",
                                  "generator_init")))
})
