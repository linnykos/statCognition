#' Evaluates value output
#'
#' @param obj value object to evaluate
#' @param target input vector (evaluted from the state functions) to evaluate
#' @param return.value boolean. If TRUE, return the numeric value of the value
#' function. If FALSE, return the integer corresponding to which action was taken
#' @param ... not used
#'
#' @return numeric
#' @export
evaluate.value <- function(obj, target, return.value = T, ...){
  stopifnot(.num_states(obj) == length(target))

  #find which block
  idx <- sapply(1:length(target), function(x){
    max(which(obj$surface$block_list[[x]] <= target[x]))
  })

  key <- paste0(idx, collapse = "-")
  val <- obj$surface$hash[[key]]
  if(return.value) val$value else val$action
}
