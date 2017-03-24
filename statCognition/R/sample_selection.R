#' Sample selection via all
#'
#' @param mat matrix
#' @param ... not used
#'
#' @return matrix
#' @export
SS_none <- function(mat, ...){
  mat
}

#' Sample selection via three standard deviations
#'
#' @param mat matrix
#' @param ... not used
#'
#' @return matrix
#' @export
SS_three_sd <- function(mat, ...){
  idx <- apply(mat, 2, function(x){
    c(which(x >= mean(x) + 3*stats::sd(x)), which(x <= mean(x) - 3*stats::sd(x)))
  })
  idx <- unique(as.vector(unlist(idx)))

  if(length(idx) == 0) mat else mat[-idx,,drop = F]
}

#' Sample selection via min/max removal
#'
#' @param mat matrix
#' @param ... not used
#'
#' @return matrix
#' @export
SS_quantile <- function(mat, ...){
  idx <- apply(mat, 2, function(x){
    c(which.min(x), which.max(x))
  })
  idx <- unique(as.vector(unlist(idx)))

  mat[-idx,,drop = F]
}

#' Sample selection via neighborhood
#'
#' @param mat matrix
#' @param quantile quantile of all the pairwise distances to be the threshold
#' @param neighbor_threshold how many neighbors or fewer needed to be considered outlier
#' @param ... not used
#'
#' @return matrix
#' @export
SS_neighborhood <- function(mat, quantile = 0.5, neighbor_threshold = 1, ...){
  mat_scale <- scale(mat)
  dis <- stats::dist(mat_scale)
  radius <- stats::quantile(dis, probs = quantile)
  nn <- dbscan::frNN(mat_scale, eps = radius)

  bool <- sapply(nn, function(x){
    if(length(x) <= neighbor_threshold) FALSE else TRUE
  })

  mat[which(bool),,drop = F]
}
