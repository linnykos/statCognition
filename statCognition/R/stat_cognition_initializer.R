#' Statistical cognition system initializer
#'
#' @param action_ll list of list of actions
#' @param state_ll list of list of states
#' @param difference_list list of difference functions
#' @param generator_init synthetic_initializer object
#'
#' @return stat_cognition_initializer object
#' @export
stat_cognition_initializer <- function(action_ll, state_ll, difference_list = NA,
                                       generator_init = synthetic_initializer()){
  stopifnot(length(action_ll) == length(state_ll))
  stopifnot(all(is.na(difference_list)) | length(difference_list) == length(state_ll))

  if(any(is.na(difference_list))){
    len <- length(action_ll)
    difference_list <- lapply(1:len, function(x){difference_states_indicator})
  }

  structure(list(action_ll = action_ll, state_ll = state_ll,
                 difference_list = difference_list, generator_init = generator_init),
            class = "stat_cognition_initializer")
}
