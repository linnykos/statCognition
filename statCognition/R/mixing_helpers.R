#' Mixing function: Shuffle columns
#'
#' @param mat data matrix
#' @param percentage_columns percentage of columns affected
#'
#' @return numeric matrix
#' @export
mixing_shuffle_columns <- function(mat, percentage_columns = 0.1){
  stopifnot(percentage_columns <= 1, is.numeric(mat), is.matrix(mat), ncol(mat) >= 2)

  idx_columns <- sample(1:ncol(mat), max(ceiling(percentage_columns * ncol(mat)), 2))
  mat[,idx_columns] <- mat[,sample(idx_columns), drop = F]

  mat
}

#' Mixing function: Shuffle rows
#'
#' @param mat data matrix
#' @param percentage_columns percentage of columns affected
#'
#' @return numeric matrix
#' @export
mixing_shuffle_rows <- function(mat, percentage_columns = 0.1){
  stopifnot(percentage_columns <= 1, is.numeric(mat), is.matrix(mat))

  idx_columns <- sample(1:ncol(mat), max(ceiling(percentage_columns * ncol(mat))), 2)
  mat[,idx_columns] <- mat[sample(1:nrow(mat)), idx_columns, drop = F]

  mat
}

#' Mixing function: Refit from Gaussian distribution
#'
#' @param mat data matrix
#'
#' @return numeric matrix
#' @export
mixing_refit_normality <- function(mat){
  stopifnot(is.numeric(mat), is.matrix(mat))

  mu <- colMeans(mat)
  Sigma <- stats::cov(mat)

  MASS::mvrnorm(nrow(mat), mu, Sigma)
}

#' Mixing function: Resample rows
#'
#' @param mat data matrix
#'
#' @return numeric matrix
#' @export
mixing_resample <- function(mat){
  stopifnot(is.numeric(mat), is.matrix(mat))

  idx <- sample(1:nrow(mat), nrow(mat), replace = T)
  mat[idx,,drop = F]
}

#' Mixing function: Swap rows
#'
#' @param mat data matrix
#' @param idx1 indicies of rows
#' @param idx2 indicies of rows
#'
#' @return numeric matrix
#' @export
mixing_swap_rows <- function(mat, idx1, idx2){
  stopifnot(length(idx1) == length(idx2), length(intersect(idx1, idx2)) == 0,
            is.numeric(mat), is.matrix(mat))

  tmp <- mat[sample(idx1),,drop = F]; tmp2 <- mat[sample(idx2),,drop = F]
  mat[idx1,,drop = F] <- tmp2; mat[idx2,,drop = F] <- tmp

  mat
}

#' Mixing function: Adjust mean
#'
#' @param mat data matrix
#' @param noise_func function to apply to each column of mat
#'
#' @return numeric matrix
#' @export
mixing_adjust_mean <- function(mat, noise_func = function(x){x + stats::sd(x)/2*stats::rnorm(length(x))}){
  stopifnot(is.numeric(mat), is.matrix(mat))
  apply(mat, 1, function(y){noise_func(y)})
}

#' Mixing function: Inflate correlation
#'
#' @param mat data matrix
#' @param percentage_columns percentage of columns affected
#'
#' @return numeric matrix
#' @export
mixing_inflate_correlation <- function(mat, percentage_columns = 0.1){
  stopifnot(percentage_columns <= 1, ncol(mat) >= 2, is.numeric(mat), is.matrix(mat))

  idx <- sample(1:ncol(mat), max(floor(percentage_columns*ncol(mat)/2)*2), 2)
  if(length(idx) <= 2) idx <- sample(ncol(mat), 2)
  idx_pairs <- matrix(idx, ncol = 2)

  for(i in 1:nrow(idx_pairs)){
    mat[,idx_pairs[i,2]] <- rowMeans(mat[,idx_pairs[i,]])
  }

  mat
}
