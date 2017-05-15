#' Statistical cognition system initializer
#'
#' @param action_ll list of list of actions
#' @param state_ll list of list of states
#' @param generator_init synthetic_initializer object
#' @param difference_list list of difference functions
#'
#' @return stat_cognition_initializer object
#' @export
stat_cognition_initializer <- function(action_ll, state_ll,
                                       generator_init = synthetic_initializer(),
                                       difference_list = NA){
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
