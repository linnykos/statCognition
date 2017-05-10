stat_cognition_initializer <- function(action_ll, state_ll, difference_list = NA,
                                       generator_init = generate_synthetic_initializer()){
  if(any(is.na(difference_list))){
    len <- length(action_ll)
    difference_list <- lapply(1:len, function(x){difference_states_indicator})
  }

  structure(list(action_ll = action_ll, state_ll = state_ll,
                 difference_list = difference_list, generator_init = generator_init),
            class = "stat_cognition_initializer")
}
