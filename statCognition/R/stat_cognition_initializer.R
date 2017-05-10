stat_cognition_initializer <- function(action_ll, state_ll, difference_list,
                                       generator_init = generate_synthetic_initializer()){
  structure(list(action_ll = action_ll, state_ll = state_ll,
                 difference_list = difference_list, generator_init = generator_init),
            class = "stat_cognition_initializer")
}
