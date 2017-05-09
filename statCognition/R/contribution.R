#right continuous
contribution <- function(breakpoints, values){
  stopifnot(length(breakpoints) == length(values), is.numeric(breakpoints),
            is.numeric(values), !is.matrix(breakpoints), !is.matrix(values))

  stopifnot(breakpoints == sort(breakpoints, decreasing = F))

  res <- .remove_duplicate_breakpoints(breakpoints, values)
  res <- .remove_duplicate_values(res$breakpoints, res$values)

  structure(list(breakpoints = res$breakpoints, values = res$values),
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
