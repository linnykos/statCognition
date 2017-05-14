.contribution_estimate <- function(loc, values1, values2 = rep(0, length(values1)),
                                   store = F){
  stopifnot(is.numeric(loc), !is.matrix(loc))
  stopifnot(is.numeric(values1), !is.matrix(values1))
  stopifnot(length(loc) == length(values1))

  values <- values1 + values2
  res <- .reorder_vector(loc, values)
  if(length(loc) >= 10) {
    coef_vec <- .fused_lasso_cv(x = res$vec1, y = res$vec2)
  } else {
    coef_vec <- .isoreg_try(x = res$vec1, y = res$vec2)
  }

  if(store){
    samples <- cbind(loc, values, values2)
    colnames(samples) <- c("breakpoints", "total_value", "contribution")
  } else samples <- NA

  #output
  contribution(breakpoints = res$vec1, values = coef_vec, samples = samples)
}


.isoreg_try <- function(x, y){
  coef_vec1 <- stats::isoreg(x, y)$yf
  coef_vec2 <- -stats::isoreg(x, -y)$yf

  if(.l2norm(coef_vec1 - y) < .l2norm(coef_vec2 - y)) coef_vec1 else coef_vec2
}
