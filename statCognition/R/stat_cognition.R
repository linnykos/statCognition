stat_cognition_initializer <- function(action_list_list, state_list,
                                       generator_init = generate_synthetic_initializer()){
  structure(list(action_list_list = action_list_list, state_list = state_list,
                 generator_init = generator_init),
            class = "stat_cognition_initializer")
}

stat_cognition <- function(mat, pheno = NA, init, seed_vec, response_vec = NA){
  num_step <- length(init$action_list_list)
  num_act_vec <- sapply(init$action_list_list, length)
  num_mat <- length(seed_vec)
  num_state <- length(init$state_list)

  mat_org <- mat; pheno_org <- pheno

  #construct mat
  res <- .initialize_cognition_setup(num_step, num_state, num_mat, num_act_vec)
  value_list_list <- res$value_list_list; idx_list <- res$idx_list
  counter <- 1

  for(i in 1:num_mat){
    mat <- .generate_synthetic_data_from_seed(mat_org, init$generator_init,
                                              base_seed = seed_vec[i])
    pheno <- pheno_org

    for(j in 1:num_step){
      #determine all possible outcomes
      res <- .apply_all_actions_return_states(mat, pheno, init$action_list_list[[j]],
                                              init$state_list)

      #reformat to fit in mat_lis_lis
      for(k in 1:nrow(res)){
        value_list_list[[j]][[k]][i,] <- res[k,]
      }

      #record action
      if(all(is.na(response_vec))){
        response <- readline(paste0("Enter the action's number that you would like ",
                                    "to perform, among: ",
                                    paste0(names(init$action_list_list[[j]]), collapse = ", ")))
      } else {
        response <- response_vec[counter]; counter <- counter + 1
      }

      idx_list[[j]][i] <- response

      #upmate the mat and pheno
      res <- init$action_list_list[[j]][[response]](mat, pheno = pheno)
      if(any(is.list(res))){mat <- res$mat; pheno <- res$pheno} else {mat <- res}
    }
  }

  #estimate the value function
  .estimate_mpj_sequence(value_list_list, idx_list)
}

.initialize_cognition_setup <- function(num_step, num_state, num_mat, num_act_vec){
  stopifnot(length(num_act_vec) == num_step)

  value_list_list <- lapply(1:num_step, function(x){
    lapply(1:num_state, function(y){
      matrix(0, nrow = num_mat, ncol = num_act_vec[x])
    })
  })

  idx_list <- lapply(1:num_step, function(x){rep(0, num_mat)})

  list(value_list_list = value_list_list, idx_list = idx_list)
}

.apply_all_actions_return_states <- function(mat, pheno = NA, action_list, state_list){
  sapply(1:length(action_list), function(x){
    .apply_action_return_state(mat, pheno, action_list[[x]], state_list)
  })
}

.apply_action_return_state <- function(mat, pheno = NA, action, state_list){
  res <- action(mat, pheno = pheno)
  if(any(is.list(res))){
    .apply_cognition_state(state_list, res$mat, pheno = res$pheno)
  } else {
    .apply_cognition_state(state_list, res, pheno = pheno)
  }
}
