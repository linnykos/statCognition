value_estimate <- function(contribution_ll, store = F){
  stopifnot(class(contribution_ll) == "list")
  stopifnot(all(sapply(contribution_ll, class) == "list"))
  stopifnot(all(unlist(sapply(contribution_ll, function(x){sapply(x, class)})) == "contribution"))
  stopifnot(length(unique(sapply(contribution_ll, length))) == 1)

  #form all the table
  block_list <- lapply(contribution_ll, .form_block)

  #expand blocks
  idx_list <- lapply(block_list, function(x){1:length(x)})
  grid <- expand.grid(idx_list)

  #fill out hash table
  h <- hash::hash()
  apply(grid, 1, function(x){.update_hash(h, contribution_ll, block_list, x)})

  if(store){
    value(list(hash = h, block_list = block_list), contribution_ll = contribution_ll)
  } else {
    value(list(hash = h, block_list = block_list))
  }

}

#############################

.form_block <- function(lis, tol = 1e-4){
  stopifnot(all(sapply(lis, class) == "contribution"))

  vec <- sort(unlist(sapply(lis, function(x){x$breakpoints})), decreasing = F)
  vec <- unique(vec[c(1, which(abs(diff(vec)) > tol)+1)])

  c(-Inf, vec)
}

.update_hash <- function(h, contribution_ll, block_list, vec){
  stopifnot(length(block_list) == length(vec))
  stopifnot(length(unique(sapply(contribution_ll, length))) == 1)

  d <- length(contribution_ll); a <- unique(sapply(contribution_ll, length))

  val <- sapply(1:length(vec), function(x){block_list[[x]][vec[x]]})

  #for each action, evaluate how much val is worth
  action_val <- sapply(1:a, function(i){
    sum(sapply(1:d, function(j){
      evaluate(contribution_ll[[j]][[i]], val[j])
    }))
  })

  val <- max(action_val); action <- which.max(action_val)
  idx <- paste0(vec, collapse = "-")
  h[[idx]] <- data.frame(value = val, action = action)

  invisible()
}
