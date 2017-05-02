.estimate_mpj_sequence <- function(mat_list_list, idx_list, weight = 0.8){
  stopifnot(length(mat_list_list) == length(idx_list))

  n <- length(idx_list)
  value_list <- vector("list", n)

  value_list[[n]] <- .estimate_mpj(mat_list_list[[n]], idx_list[[n]])
  for(i in (n-1):1){
    res <- .estimate_mpj(mat_list_list[[i]], idx_list[[i]])
    value_list[[i]] <- .add_mpj(value_list[[n]], res, weight2 = weight^(n-i))
  }

  structure(value_list, class = "mpj_sequence")
}
