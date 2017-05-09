.contribution_estimate <- function(loc, val){
  stopifnot(is.numeric(loc), !is.matrix(loc), is.numeric(val), !is.matrix(val))
  stopifnot(length(loc) == length(loc))

  res <- .reorder_vector(loc, val)
  coef_vec <- .fused_lasso_cv(x = res$vec1, y = res$vec2)

  #output
  contribution(breakpoints = res$vec1, values = coef_vec)
}
