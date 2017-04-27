#right continuous
.monotone_piecewise_marginal <- function(breakpoints, values){
  stopifnot(length(breakpoints) == length(values), is.numeric(breakpoints),
            is.numeric(values), !is.matrix(breakpoints), !is.matrix(values))

  stopifnot(all(values == sort(values, decreasing = F),
                breakpoints == sort(breakpoints, decreasing = F)))

  structure(list(breakpoints = breakpoints, values = values), class = "mpm")
}
