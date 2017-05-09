#right continuous
contribution <- function(breakpoints, values, data = NA){
  stopifnot(length(breakpoints) == length(values), is.numeric(breakpoints),
            is.numeric(values), !is.matrix(breakpoints), !is.matrix(values))

  stopifnot(breakpoints == sort(breakpoints, decreasing = F))

  res <- .remove_duplicates(breakpoints, values)

  structure(list(breakpoints = res$breakpoints, values = res$values, data = data),
            class = "contribution")
}

##################

.remove_duplicates <- function(breakpoints, values, tol = 1e-4){
  stopifnot(length(breakpoints) == length(values))
  n <- length(breakpoints)

  #find the breakpoint defined as the last location before a change
  idx <- n - rev(.find_breakpoints(rev(breakpoints))) + 1
  idx <- unique(c(idx, n))

  list(breakpoints = breakpoints[idx], values = values[idx])
}

#defined as right breakpoint, i.e., c(1,1,2) has a breakpoint at 3 not 2
.find_breakpoints <- function(val, tol = 1e-4){
  idx <- which(abs(diff(val)) >= tol)
  if(length(idx) > 0) return(idx + 1) else return(1)
}
