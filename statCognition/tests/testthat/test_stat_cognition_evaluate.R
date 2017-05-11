context("Test stat cognition evaluation")

## stat_cognition_evaluate is correct

test_that("stat_cognition_evaluate works", {
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

  cog <- stat_cognition(dat, init, seed_vec, response_vec)
  res <- evaluate(cog, dat)

  expect_true(class(res) == "stat_cognition_result")
  expect_true(all(names(res) == c("pipeline", "result")))
  expect_true(length(res$pipeline) == 2)
  expect_true(all(res$pipeline %% 1 == 0))
  expect_true(all(res$pipeline > 0))
  expect_true(length(names(res$pipeline)) > 0)
  expect_true(is.logical(res$result))

})
