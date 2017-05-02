stat_cognition_initializer <- function(action_list_list, state_list){
  structure(list(action_list_list = action_list_list, state_list = state_list),
            class = "stat_cognition_initializer")
}

stat_cognition <- function(dat, pheno = NA, init, seed_vec){
  num_step <- length(init$action_list_list)
  num_act_vec <- sapply(init$action_list_list, length)
  num_dat <- length(seed_vec)
  num_state <- length(init$state_list)

  dat_org <- dat; pheno_org <- pheno

  #construct mat
  res <- .initialize_cognition_setup(num_step, num_state, num_dat, num_act_vec)
  mat_lis_lis <- res$mat_lis_lis; idx_lis <- res$idx_lis

  for(i in 1:num_dat){
    dat <- dat_org; pheno <- pheno_org

    for(j in 1:num_step){
      #determine all possible outcomes
      res <- .apply_all_actions_return_states(dat, pheno, init$action_list_list[[j]],
                                              init$state_list)

      #reformat to fit in mat_lis_lis
      for(k in 1:nrow(res)){
        mat_lis_lis[[j]][[k]][i,] <- res[k,]
      }

      #record action
      response <- readline(paste0("Enter the action's number that you would like ",
                                  "to perform, among: ",
                                  paste0(names(init$action_list_list[[j]], collapse = ", "))))
      idx_lis[[j]][i] <- response

      #update the dat and pheno
      res <- init$action_list_list[[j]][[response]](dat, pheno = pheno)
      if(any(is.list(res))){dat <- res$dat; pheno <- res$pheno} else {dat <- res}
    }
  }

  #estimate the value function
}

.initialize_cognition_setup <- function(num_step, num_state, num_dat, num_act_vec){
  stopifnot(length(num_act_vec) == num_step)

  mat_lis_lis <- lapply(1:num_step, function(x){
    lapply(1:num_state, function(y){
      matrix(0, nrow = num_dat, ncol = num_act_vec[x])
    })
  })

  idx_lis <- lapply(1:num_step, function(x){rep(0, num_dat)})

  list(mat_lis_lis = mat_lis_lis, idx_lis = idx_lis)
}

.apply_all_actions_return_states <- function(dat, pheno = NA, action_list, state_list){
  t(sapply(1:length(action_list), function(x){
    .apply_action_return_state(dat, pheno, action_list[[x]], state_list)
  }))
}

.apply_action_return_state <- function(dat, pheno = NA, action, state_list){
  res <- action(dat, pheno = pheno)
  if(any(is.list(res))){
    .apply_cognition_state(state_list, res$mat, res$pheno)
  } else {
    .apply_cognition_state(state_list, res)
  }
}
