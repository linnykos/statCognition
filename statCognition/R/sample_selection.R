#' Sample selection via all
#'
#' @param dat data object
#' @param ... not used
#'
#' @return list containing the modified \code{mat} and \code{pheno}
#' @export
SS_none <- function(dat, ...){
  dat
}

#' Sample selection via neighborhood
#'
#' @param dat data object
#' @param quantile quantile of all the pairwise distances to be the threshold
#' @param neighbor_threshold how many neighbors or fewer needed to be considered outlier
#' @param ... not used
#'
#' @return list containing the modified \code{mat} and \code{pheno}
#' @export
SS_neighborhood <- function(dat, quantile = 0.5, neighbor_threshold = 1, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data")

  mat_scale <- scale(dat$mat)
  dis <- stats::dist(mat_scale)
  rad <- stats::quantile(dis, probs = quantile)
  nn <- dbscan::frNN(mat_scale, eps = rad)

  bool <- sapply(nn$id, function(x){
    if(length(x) <= neighbor_threshold) FALSE else TRUE
  })

  dat$mat <- dat$mat[which(bool),,drop = F]
  if("pheno" %in% names(dat)) dat$pheno <- dat$pheno[which(bool),,drop = F]

  dat
}

#' Sample selection via Cook's distance
#'
#' Applies many pairwise (simple) regression and removes points that have
#' too high of a Cook's distance
#'
#' @param dat data object
#' @param pairs maximum number of pairs to look at
#' @param ... not used
#'
#' @return
#' @export
#'
#' @examples
SS_cook <- function(dat, pairs = 50, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data")

  d <- ncol(dat$mat)
  pairs_mat <- utils::combn(d, 2); pairs_mat <- pairs_mat[,1:min(ncol(pairs_mat), pairs)]

  idx <- unique(unlist(apply(pairs_mat, 2, function(x){
    tmp_mat <- dat$mat[,x]; colnames(tmp_mat) <- c("V1", "V2"); tmp_mat <- as.data.frame(tmp_mat)
    res <- stats::lm(V1~V2, data = tmp_mat)
    vec <- stats::cooks.distance(res)
    cutoff <- 1.5*stats::IQR(vec) + stats::median(vec)
    which(vec > cutoff)
  })))

  if(length(idx) != 0){
    dat$mat <- dat$mat[-idx,,drop = F]
    if("pheno" %in% names(dat)) dat$pheno <- dat$pheno[-idx,,drop = F]
  }

  dat
}
