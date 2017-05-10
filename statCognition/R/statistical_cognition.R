stat_cognition <- function(dat, init, seed_vec, response_vec = NA, ...){
  num_step <- length(init$action_ll)
  num_act_vec <- sapply(init$action_ll, length)
  num_mat <- length(seed_vec)
  num_state_vec <- sapply(init$state_ll, length)

  dat_org <- dat
  state_action_ll <- lapply(1:num_mat, function(x){
    vector("list", num_step)
  })

  for(i in 1:num_mat){
    dat <- .generate_synthetic_data_from_seed(dat_org, init$generator_init,
                                              base_seed = seed_vec[i])
    prev_dat <- dat

    for(j in 1:num_step){
      #record action
      if(all(is.na(response_vec))){
        response <- readline(paste0("Enter the action's number that you would like ",
                                    "to perform, among: ",
                                    paste0(names(init$action_ll[[j]]), collapse = ", ")))
      } else {
        response <- response_vec[counter]; counter <- counter + 1
      }

      #upmate dat
      state_action_ll[[i]][[j]] <- .store_state_action(dat, init$action_ll[[j]],
                                                       init$state_ll[[j]], init$state_ll[[j+1]],
                                                       prev_dat, ...)
      prev_dat <- dat
      dat <- init$action_ll[[j]][[response]](dat, ...)
    }
  }

  .estimate_value_cognition(state_action_ll, init, seed_vec)
}

#############

# work backword, forming loc-idx for dimension-action, contribution_ll and
## value function for each step
.estimate_value_cognition <- function(state_action_ll, init, seed_vec){

  num_step <- length(init$action_ll); num_mat <- length(seed_vec)
  num_act_vec <- sapply(init$action_ll, length)
  num_state_vec <- sapply(init$state_ll, length)

  value_list <- vector("list", length(init$action_ll)); val <- NA

  for(i in num_step:1){
    locidx_ll <- lapply(1:num_state_vec[i], function(x){
      lapply(1:num_act_vec[i], function(x){matrix(0, num_mat, 2)})
    })

    #iterate over all synthetic data
    for(j in 1:num_mat){
      tmp <- .value_contribution(state_action_ll[[j]][[i]], val = val,
                                 diff_func = init$difference_list[[i]])

      #transfer
      for(k in 1:num_state_vec[i]){ for(l in 1:num_act_vec[i]){
          locidx_ll[[k]][[l]][j,] <- tmp[[k]][[l]][1,]
        }}
    }

    #estimate contribution_ll
    contribution_ll <- lapply(1:num_state_vec[i], function(x){
      lapply(1:num_act_vec[i], function(y){
        .contribution_estimate(locidx_ll[[k]][[l]][,1], locidx_ll[[k]][[l]][,2])})
    })

    value_list[[i]] <- value_estimate(contribution_ll)
    val <- value_list[[i]]
  }

  value_list
}
