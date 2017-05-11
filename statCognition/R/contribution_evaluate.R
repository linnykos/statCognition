#' Evaluates contribution output
#'
#' @param obj contribution object to evaluate
#' @param target numeric (scalar) to evaluate
#' @param ... not used
#'
#' @return numeric
#' @export
evaluate.contribution <- function(obj, target, ...){
  stopifnot(is.numeric(target), length(target) == 1)

  idx <- which(obj$breakpoints <= target)
  if(length(idx) == 0) return(obj$values[1]) else return(obj$values[max(idx)])
}
