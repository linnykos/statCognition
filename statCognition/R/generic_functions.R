#' Checks object validity
#'
#' Generic function that fails noisily with a stop message if the object is
#' invalid. Otherwise, returns TRUE
#'
#' @param obj object to check
#' @param ... not used
#'
#' @return boolean
#' @export
is_valid <- function(obj, ...) UseMethod("is_valid")

#' Evaluates an object
#'
#' @param obj object to evaluate
#' @param target input to evaluate the obj at
#' @param ... not used
#'
#' @return vector
#' @export
evaluate <- function(obj, target, ...) UseMethod("evaluate")
