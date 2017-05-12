.contribution_estimate <- function(loc, val){
  stopifnot(is.numeric(loc), !is.matrix(loc), is.numeric(val), !is.matrix(val))
  stopifnot(length(loc) == length(loc))

  res <- .reorder_vector(loc, val)
  if(length(loc) >= 10) {
    coef_vec <- .fused_lasso_cv(x = res$vec1, y = res$vec2)
  } else {
    coef_vec <- .isoreg_try(x = res$vec1, y = res$vec2)
  }

  #output
  contribution(breakpoints = res$vec1, values1 = coef_vec)
}


.isoreg_try <- function(x, y){
  coef_vec1 <- stats::isoreg(x, y)$yf
  coef_vec2 <- -stats::isoreg(x, -y)$yf

  if(.l2norm(coef_vec1 - y) < .l2norm(coef_vec2 - y)) coef_vec1 else coef_vec2
}
