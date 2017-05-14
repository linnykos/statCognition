#' Pairwise depedency: Pearson
#'
#' @param dat data object with only 2 columns
#' @param threshold threshold
#'
#' @return boolean
#' @export
PD_pearson <- function(dat, threshold = 0.35){
  stopifnot("mat" %in% names(dat), ncol(dat$mat) == 2)
  res <- stats::cor(dat$mat[,1], dat$mat[,2])
  res > threshold
}

#' Pairwise depedency: Kendall's tau
#'
#' @param dat data object with only 2 columns
#' @param threshold threshold
#'
#' @return boolean
#' @export
PD_kendall <- function(dat, threshold = 0.35){
  stopifnot("mat" %in% names(dat), ncol(dat$mat) == 2)
  res <- stats::cor(dat$mat[,1], dat$mat[,2], method = "kendall")
  res > threshold
}

#' Pairwise depedency: Energy distance
#'
#' @param dat data object with only 2 columns
#' @param threshold threshold
#'
#' @return boolean
#' @export
PD_energy <- function(dat, threshold = 0.35){
  stopifnot("mat" %in% names(dat), ncol(dat$mat) == 2)
  res <- energy::dcor(dat$mat[,1], dat$mat[,2])
  res > threshold
}
