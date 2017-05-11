#' Checks object validity
#'
#' Generic function that fails noisily with a stop message if the object is
#' invalid. Otherwise, returns TRUE
#'
#' @param obj The object to check
#' @param ... not used
#' @return boolean
#' @export
is_valid <- function(obj, ...) UseMethod("is_valid")
