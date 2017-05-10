#' Mixing function: Refit from Gaussian distribution
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows to refit
#' @param ... not used
#'
#' @return data object
#' @export
mixing_refit_normality <- function(dat, param1 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  mu <- colMeans(dat$mat)
  Sigma <- stats::cov(dat$mat)

  n <- nrow(dat$mat)
  idx <- sample(1:n, ceiling(max(1, param1*n)))

  dat$mat[idx,] <- MASS::mvrnorm(length(idx), mu, Sigma)
  dat
}
