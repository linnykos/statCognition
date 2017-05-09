#' State feature: variance along PC's
#'
#' @param dat data object
#' @param num_pc number of principal components to consider
#' @param ... not used
#'
#' @return
#' @export
state_variance <- function(dat, num_pc = 1, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data", num_pc <= ncol(dat$mat))

  res <- stats::princomp(dat$mat)
  sum(res$sdev[1:num_pc])/sum(res$sdev)
}

#' State feature: interpoint distance
#'
#' Computes the ratio between the maximum (minimum distance to nearest point)
#' over minimum (minimum distance to nearest point)
#'
#' @param dat data object
#' @param ... not used
#'
#' @return value
#' @export
state_interpoint <- function(dat, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data")

  dis <- as.matrix(stats::dist(dat$mat))
  diag(dis) <- Inf

  vec <- sapply(1:ncol(dis), function(x){
    min(dis[x,])
  })

  max(vec)/min(vec)
}
