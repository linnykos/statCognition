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
#' @param multiplier multiplier of the IQR
#' @param ... not used
#'
#' @return
#' @export
#'
#' @examples
SS_cook <- function(dat, multiplier = 2, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data")

  d <- ncol(dat$mat); df <- as.data.frame(dat$mat); colnames(df) <- paste0("V", 1:d)

  idx <- unique(unlist(sapply(1:d, function(x){
    res <- stats::lm(paste0(colnames(df)[x],"~."), data = df)
    vec <- stats::cooks.distance(res)
    cutoff <- multiplier*stats::IQR(vec) + stats::median(vec)
    which(vec > cutoff)
  })))

  if(length(idx) != 0){
    dat$mat <- dat$mat[-idx,,drop = F]
    if("pheno" %in% names(dat)) dat$pheno <- dat$pheno[-idx,,drop = F]
  }

  dat
}
