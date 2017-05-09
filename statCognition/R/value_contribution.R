.value_contribution <- function(dat, action_list, state_list, state_list_future, outcome_state, val,
                               diff_func = difference_states_indicator, ...){
  a <- length(action_list); d <- length(state_list)
  grid <- expand.grid(1:a, 1:d)

  res_ll <- lapply(1:d, function(x){lapply(1:a, function(y){
    future_dat <- action_list[[y]](dat, ...)
    v <- value_evaluate(val, .state_extract(future_dat, state_list_future, prev_dat = dat, ...))
    idx <- diff_func(future_dat, outcome_state)
    loc <- state_list[[x]](dat, ...)
    c(loc, idx+v)
  })})

  res_ll
}

.state_extract <- function(dat, state_list, ...){
  d <- length(state_list)
  sapply(1:d, function(x){state_list[[x]](dat, ...)})
}
