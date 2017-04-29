#estimating
.estimate_mpm <- function(mat, idx){
  stopifnot(is.numeric(mat), is.matrix(mat), is.numeric(idx), !is.matrix(idx))
  stopifnot(nrow(mat) == length(idx), ncol(mat) >= max(idx), min(idx) >= 1)
  stopifnot(all(idx %% 1 == 0))

  #sort each row first
  res <- .sort_matrix(mat, idx); mat <- res$mat; idx <- res$idx

  #assign values to each row of matrix
  val <- t(apply(cbind(mat, idx), 1, .assign_values_to_mpm))

  #remove values
  res <- .remove_zero_values(mat, idx, val)
  x <- res$x; y <- res$y

  #fit isotonic regression
  res <- stats::isoreg(x = x, y = y)

#output
  .monotone_piecewise_marginal(x, res$yf, data = y)
}

#sorts rows of a matrix and updates idx appropriately
.sort_matrix <- function(mat, idx){
  for(i in 1:nrow(mat)){
    vec <- rank(mat[i,])
    idx[i] <- vec[idx[i]]
    mat[i,] <- sort(mat[i,], decreasing = F)
  }

  list(mat = mat, idx = idx)
}

#last value is the idx
.assign_values_to_mpm <- function(vec){
  idx <- vec[length(vec)]; vec <- vec[-length(vec)]; n <- length(vec)
  val <- rep(0, n)

  val[idx:n] <- (idx-1)/(n-1)
  val
}

#removes zeros that come after (i.e., larger) the smallest accept value
.remove_zero_values <- function(mat, idx, val){
  vec_mat <- as.numeric(mat)
  vec_val <- as.numeric(val)

  vec_val <- vec_val[order(vec_mat)]; vec_mat <- sort(vec_mat)

  vec_selected <- sapply(1:nrow(mat), function(x){mat[x,idx[x]]})
  idx_rejected <- which(vec_val == 0); vec_rejected <- vec_mat[idx_rejected]
  idx_remove <- which(vec_rejected >= min(vec_selected))

  if(length(idx_remove) > 0){
    vec_val <- vec_val[-idx_rejected[idx_remove]]
    vec_mat <- vec_mat[-idx_rejected[idx_remove]]
  }

  list(x = vec_mat, y = vec_val)
}

