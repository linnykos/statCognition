.value_contribution <- function(state_action, val = NA, diff_func = difference_states_indicator){
  a <- length(state_action$future); d <- length(state_action$current)
  grid <- expand.grid(1:a, 1:d)

  res_ll <- lapply(1:d, function(x){
    res <- lapply(1:a, function(y){
      if(!any(is.na(val))) {
        v <- value_evaluate(val, state_action$future[[y]])
      } else {v <- 0}
      idx <- diff_func(state_action$future[[y]],
                       state_action$future[[state_action$response]])
      loc <- state_action$current[x]
      c(as.numeric(loc), idx+v)})
    names(res) <- names(state_action$future)
    res
  })
  names(res_ll) <- names(state_action$current)

  res_ll
}

#?? test state_list_func = na
.store_state_action <- function(dat, action_list, state_list, state_list_future = NA,
                                prev_dat, response, ...){

  current <- .state_extract(dat, state_list, prev_dat = prev_dat)
  names(current) <- names(state_list)

  future <- lapply(1:length(action_list), function(x){
    future_dat <- action_list[[x]](dat, ...)
    if(!any(is.na(state_list_future))) {
      .state_extract(future_dat, state_list_future, prev_dat = dat)
    } else { future_dat }
  })
  names(future) <- names(action_list)

  res <- structure(list(current = current, future = future, response = response),
            class = "state_action")
  is_valid(res)
  res
}

.state_extract <- function(dat, state_list, ...){
  d <- length(state_list)
  sapply(1:d, function(x){set.seed(10); state_list[[x]](dat, ...)})
}


#' Checks state_action object for validity
#'
#' @param obj The object to check
#' @param ... not used
#'
#' @return boolean
#' @export
is_valid.state_action <- function(obj, ...){
  stopifnot(length(obj) == 3, all(names(obj) == c("current", "future", "response")))
  stopifnot(length(unique(sapply(obj$future, length))) == 1)

  TRUE
}
