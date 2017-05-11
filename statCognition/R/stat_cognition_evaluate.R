#' Evaluates stat cognition output
#'
#' @param obj stat_cognition object to evaluate
#' @param target input data object to evaluate
#' @param ... not used
#'
#' @return stat_cognition_result object
#' @export
evaluate.stat_cognition<- function(obj, target, ...){
  stopifnot(class(target) == "data")

  num_step <- length(obj$action_ll)
  num_act_vec <- sapply(obj$action_ll, length)
  num_state_vec <- sapply(obj$state_ll, length)

  prev_target <- target
  pipeline <- rep(0, num_step)
  nam_vec <- rep(0, num_step)

  for(i in 1:num_step){
    state <- .state_extract(target, obj$state_ll[[i]], prev_dat = prev_target)
    pipeline[i] <- evaluate(obj$value_list[[i]], state, return.value = F)
    nam_vec[i] <- names(obj$action_ll[[i]])[pipeline[i]]
    prev_target <- target

    target <- obj$action_ll[[i]][[pipeline[i]]](target)
  }
  names(pipeline) <- nam_vec

  structure(list(pipeline = pipeline, result = target), class = "stat_cognition_result")
}
