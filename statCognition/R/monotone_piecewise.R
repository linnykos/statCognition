#right continuous
.monotone_piecewise_marginal <- function(breakpoints, values, data = NA){
  stopifnot(length(breakpoints) == length(values), is.numeric(breakpoints),
            is.numeric(values), !is.matrix(breakpoints), !is.matrix(values))

  stopifnot(all(values == sort(values, decreasing = F),
                breakpoints == sort(breakpoints, decreasing = F)))

  structure(list(breakpoints = breakpoints, values = values, data = data),
            class = "mpm")
}

.monotone_piecewise_joint <- function(breakpoints_list, values_list){
  stopifnot(length(breakpoints_list) == length(values_list))

  mpm_list <- lapply(1:length(breakpoints_list), function(x){
    .monotone_piecewise_marginal(breakpoints_list[[x]], values_list[[x]])
  })

  .mpj_from_mpm(mpm_list)
}

.mpj_from_mpm <- function(mpm_list){
  stopifnot(class(mpm_list) == "list")

  structure(mpm_list, class = "mpj")
}
