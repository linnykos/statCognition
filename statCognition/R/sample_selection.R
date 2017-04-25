#' Sample selection via all
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
SS_none <- function(mat, pheno = NA, ...){
  list(mat = mat, pheno = pheno)
}

#' Sample selection via three standard deviations
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
SS_three_sd <- function(mat, pheno = NA, ...){
  idx <- apply(mat, 2, function(x){
    c(which(x >= mean(x) + 3*stats::sd(x)), which(x <= mean(x) - 3*stats::sd(x)))
  })
  idx <- unique(as.vector(unlist(idx)))

  if(length(idx) != 0){
    mat <- mat[-idx,,drop = F]
    if(!any(is.na(pheno))) pheno <- pheno[-idx,,drop = F]
  }

  list(mat = mat, pheno = pheno)
}

#' Sample selection via min/max removal
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
SS_quantile <- function(mat, pheno = NA, ...){
  idx <- apply(mat, 2, function(x){
    c(which.min(x), which.max(x))
  })
  idx <- unique(as.vector(unlist(idx)))

  if(length(idx) != 0){
    mat <- mat[-idx,,drop = F]
    if(!any(is.na(pheno))) pheno <- pheno[-idx,,drop = F]
  }

  list(mat = mat, pheno = pheno)
}

#' Sample selection via neighborhood
#'
#' @param mat matrix
#' @param pheno data frame
#' @param quantile quantile of all the pairwise distances to be the threshold
#' @param neighbor_threshold how many neighbors or fewer needed to be considered outlier
#' @param ... not used
#'
#' @return matrix
#' @export
SS_neighborhood <- function(mat, pheno = NA, quantile = 0.5, neighbor_threshold = 1, ...){
  mat_scale <- scale(mat)
  dis <- stats::dist(mat_scale)
  rad <- stats::quantile(dis, probs = quantile)
  nn <- dbscan::frNN(mat_scale, eps = rad)

  bool <- sapply(nn$id, function(x){
    if(length(x) <= neighbor_threshold) FALSE else TRUE
  })

  mat <- mat[which(bool),,drop = F]
  if(!any(is.na(pheno))) pheno <- pheno[which(bool),,drop = F]

  list(mat = mat, pheno = pheno)
}
