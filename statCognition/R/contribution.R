#' Contribution object
#'
#' Each of these functions are right continuous
#'
#' @param breakpoints vector of numerics (breakpoints, x-direction), must be sorted
#' @param values1 vector of numerics (downstream value)
#' @param values2 vector of numerics (contribution)
#' @param store boolean on whether or not to store
#'
#' @return
#' @export
#'
#' @examples
contribution <- function(breakpoints, values1, values2 = rep(0, length(values1)),
                         store = F){
  stopifnot(length(breakpoints) == length(values1), is.numeric(breakpoints),
            is.numeric(values1), !is.matrix(breakpoints), !is.matrix(values1))
  stopifnot(length(values2) == length(values1), is.numeric(values2), !is.matrix(values2))
  stopifnot(breakpoints == sort(breakpoints, decreasing = F))

  values <- values1 + values2

  res <- .remove_duplicate_breakpoints(breakpoints, values)
  res <- .remove_duplicate_values(res$breakpoints, res$values)

  if(store){
    mat <- cbind(breakpoints, values, values2)
    colnames(mat) <- c("breakpoints", "total_value", "contribution")
  } else {
    mat <- NA
  }

  structure(list(breakpoints = res$breakpoints, values = res$values, samples = mat),
            class = "contribution")
}

##################

#remove breakpoints that are too close to one another
.remove_duplicate_breakpoints <- function(breakpoints, values, tol = 1e-4){
  stopifnot(length(breakpoints) == length(values))
  n <- length(breakpoints)

  #find the breakpoint defined as the last location before a change
  idx <- n - rev(.find_breakpoints(rev(breakpoints))) + 1
  idx <- unique(c(idx, n))

  list(breakpoints = breakpoints[idx], values = values[idx])
}

#remove duplicated adjacent values
.remove_duplicate_values <- function(breakpoints, values, tol = 1e-4){
  dif <- diff(values)
  idx <- c(1, which(abs(dif) > tol) + 1)

  list(breakpoints = breakpoints[idx], values = values[idx])
}

#defined as right breakpoint, i.e., c(1,1,2) has a breakpoint at 3 not 2
.find_breakpoints <- function(val, tol = 1e-4){
  idx <- which(abs(diff(val)) >= tol)
  if(length(idx) > 0) return(idx + 1) else return(1)
}
