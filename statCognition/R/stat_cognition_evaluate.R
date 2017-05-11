stat_cognition_evaluate <- function(obj, dat){
  stopifnot(class(obj) == "stat_cognition")

  num_step <- length(obj$action_ll)
  num_act_vec <- sapply(obj$action_ll, length)
  num_state_vec <- sapply(obj$state_ll, length)

  prev_dat <- dat
  pipeline <- rep(0, num_step)
  nam_vec <- rep(0, num_step)

  for(i in 1:num_step){
    state <- .state_extract(dat, obj$state_ll[[i]], prev_dat = prev_dat)
    pipeline[i] <- value_evaluate(obj$value_list[[i]], state, return.value = F)
    nam_vec[i] <- names(obj$action_ll[[i]])[pipeline[i]]
    prev_dat <- dat

    dat <- obj$action_ll[[i]][[pipeline[i]]](dat)
  }
  names(pipeline) <- nam_vec

  structure(list(pipeline = pipeline, result = dat), class = "stat_cognition_result")
}
