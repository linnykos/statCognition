stat_cognition <- function(dat, init, seed_vec, response_vec = NA, verbose = F){
  num_step <- length(init$action_ll)
  num_act_vec <- sapply(init$action_ll, length)
  num_mat <- length(seed_vec)
  num_state_vec <- sapply(init$state_ll, length)

  dat_org <- dat; counter <- 1
  state_action_ll <- lapply(1:num_mat, function(x){
    vector("list", num_step)
  })

  for(i in 1:num_mat){
    if(verbose) print(paste0("Working on dataset ",i))

    dat <- .synthetic_generator_seed(dat_org, init$generator_init, seed = seed_vec[i])
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
      if(j != num_step) state_ll <- init$state_ll[[j+1]] else state_ll <- NA
      state_action_ll[[i]][[j]] <- .store_state_action(dat, init$action_ll[[j]],
                                                       init$state_ll[[j]], state_ll,
                                                       prev_dat, response)
      prev_dat <- dat
      dat <- init$action_ll[[j]][[response]](dat)
    }
  }

  value_list <- .estimate_value_cognition(state_action_ll, init, verbose = verbose)

  structure(list(value_list = value_list, action_ll = init$action_ll,
                 state_ll = init$state_ll), class = "stat_cognition")
}

#############

# work backword, forming loc-idx for dimension-action, contribution_ll and
## value function for each step
.estimate_value_cognition <- function(state_action_ll, init, verbose = F){

  num_step <- length(init$action_ll); num_mat <- length(state_action_ll)
  num_act_vec <- sapply(init$action_ll, length)
  num_state_vec <- sapply(init$state_ll, length)

  value_list <- vector("list", length(init$action_ll)); val <- NA

  for(i in num_step:1){
    if(verbose) print(paste0("Fitting iteration ", i))

    locidx_ll <- lapply(1:num_state_vec[i], function(x){
      lapply(1:num_act_vec[i], function(x){matrix(0, num_mat, 2)})
    })

    #iterate over all synthetic data
    for(j in 1:num_mat){
      tmp <- .value_contribution(state_action_ll[[j]][[i]], val = val,
                                 diff_func = init$difference_list[[i]])

      #transfer
      for(k in 1:num_state_vec[i]){ for(l in 1:num_act_vec[i]){
          locidx_ll[[k]][[l]][j,] <- tmp[[k]][[l]]
        }}
    }

    #estimate contribution_ll
    contribution_ll <- lapply(1:num_state_vec[i], function(k){
      lapply(1:num_act_vec[i], function(l){
        print(paste0(k, ":", l))
        .contribution_estimate(locidx_ll[[k]][[l]][,1], locidx_ll[[k]][[l]][,2])})
    })

    value_list[[i]] <- value_estimate(contribution_ll)
    val <- value_list[[i]]
  }

  value_list
}
