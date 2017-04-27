#estimating
.estimate_mpm <- function(mat, idx){
  stopifnot(is.numeric(mat), is.matrix(mat), is.numeric(idx), !is.matrix(idx))
  stopifnot(nrow(mat) == length(idx), ncol(mat) >= max(idx), min(idx) >= 1)
  stopifnot(all(idx %% 1 == 0))

  #sort each row first
  res <- .sort_matrix(mat, idx); mat <- res$mat; idx <- res$idx

  #sort all the rows together
  vec <- sort(as.numeric(mat), decreasing = F)

  #hashtable
  htable <- hash::hash(keys = as.character(vec), values = 1:length(vec))

  #form LP
  res <- .form_lp_mpm(mat, idx, htable, length(vec))
  c_mat <- res$constraint_mat; c_vec <- res$constraint_vec; obj_vec <- res$obj_vec
  res <- lpSolve::lp("max", obj_vec, c_mat, rep("<=", nrow(c_mat)), c_vec)

  #output
  .monotone_piecewise_marginal(vec, res$solution)
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

.form_lp_mpm <- function(mat, idx, htable, n, tol = 0.2){
  lis_const_mat <- vector("list", nrow(mat) + 3)

  tmp <- cbind(mat, idx); colnames(tmp) = 1:ncol(tmp)
  lis_const_mat[1:nrow(mat)] <- plyr::alply(tmp, 1, .form_lp_constraint_byrow, htable, n)
  lis_const_mat[[nrow(mat) + 1]] <- .form_monotonic_constraints(n)
  lis_const_mat[[nrow(mat) + 2]] <- .form_bounded_above_constraint(n)
  lis_const_mat[[nrow(mat) + 3]] <- .form_bounded_below_constraint(n)

  constraint_mat <- do.call(rbind, lis_const_mat)
  constraint_vec <- c(rep(tol, nrow(constraint_mat) - (n-1) - 2),
                      rep(0, n-1), 1, 0)

  obj_vec <- .form_lp_obj(mat, idx, htable, n)

  list(constraint_mat = constraint_mat, constraint_vec = constraint_vec,
       obj_vec = obj_vec)
}

#last element is the idx
.form_lp_constraint_byrow <- function(val, htable, n){
  idx <- val[length(val)]; val <- val[-length(val)]; m <- length(val)
  stopifnot(idx %% 1 == 0, idx <= m, idx >= 1)
  stopifnot(all(val == sort(val, decreasing = F)))

  if(idx == m) return(numeric(0))
  position_1 <- htable[[as.character(val[idx])]]
  position_2 <- htable[[as.character(val[m])]]

  vec <- rep(0, n)
  vec[c(position_1, position_2)] <- c(-1,1)

  vec
}

.form_monotonic_constraints <- function(n){
  t(sapply(1:(n-1), function(x){
    vec <- rep(0, n)
    vec[c(x,x+1)] <- c(1,-1)
    vec
  }))
}

.form_bounded_above_constraint <- function(n){
  vec <- rep(0, n); vec[n] <- 1; vec
}

.form_bounded_below_constraint <- function(n){
  vec <- rep(0, n); vec[1] <- -1; vec
}

.form_lp_obj <- function(mat, idx, htable, n){
  vec <- rep(0, n)
  for(i in 1:nrow(mat)){
    if(idx[i] == 1) next()
    position_1 <- htable[[as.character(mat[i, idx[i]])]]
    position_2 <- htable[[as.character(mat[i, idx[i] - 1])]]
    vec[c(position_1, position_2)] <- c(1, -1)
  }

  vec
}
