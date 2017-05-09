#' Value function
#'
#' \code{surface} is a list that contains \code{hash} and \code{block_list}.
#' \code{hash} is a hash table with keys for example "1-5" (if there are two
#' state elements, meaning the currently evaluated region lies in the first block
#' of state element 1 and the fifth block of state element 5) and has a value that
#' is a data frame with \code{value} set to the value of that block and \code{action}
#' of the action taken in that block. \code{block_list} is a list of vectors with length
#' equal to the number of state elements (2 in this case). It tells which regions in the
#' domain of the state element correspond to which blocks.
#'
#' @param surface list of \code{hash} and \code{block_list}
#'
#' @return value object
#' @export
value <- function(surface){
  res <- structure(list(surface = surface), class = "value")

  is_valid(res)
  res
}

is_valid.value <- function(obj, ...){
  stopifnot(all(names(obj) == "surface"))
  stopifnot(all(names(obj$surface) == c("hash", "block_list")))

  stopifnot(class(obj$surface$hash) == "hash")
  stopifnot(all(sapply(obj$surface$block_list, class) == "numeric"))
  stopifnot(all(sapply(obj$surface$block_list, is.matrix) == FALSE))

  action <- as.numeric(unlist(hash::values(obj$surface$hash)[2,]))
  stopifnot(min(action) >= 1)

  TRUE
}

