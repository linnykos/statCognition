#' Synthetic generator function: Refit from Gaussian distribution
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows to refit
#' @param ... not used
#'
#' @return data object
#' @export
generator_refit_normality <- function(dat, param1 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  mu <- colMeans(dat$mat)
  Sigma <- stats::cov(dat$mat)

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  idx <- sample(1:n, ceiling(max(2, param1*n)))

  dat$mat[idx,] <- tryCatch({MASS::mvrnorm(length(idx), mu, Sigma)},
                            error = function(e){
                              MASS::mvrnorm(length(idx), mu, diag(d))
                            })
  dat
}

#' Synthetic generator function: Resample rows
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows
#' @param ... not used
#'
#' @return data object
#' @export
generator_resample <- function(dat, param1 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat)
  idx <- sample(1:n, ceiling(max(2, param1*n)))

  dat$mat[idx,] <- dat$mat[sample(idx, replace = T),]
  dat
}

#' Synthetic generator function: Add Gaussian noise to columns
#'
#' @param dat data object
#' @param param1 parameter for percentage of columns
#' @param param2 parameter for percentage of rows
#' @param param3 parameter for mean of noise
#' @param param4 parameter for sd of noise
#' @param ... not used
#'
#' @return data object
#' @export
generator_add_noise <- function(dat, param1 = c(0, 1), param2 = c(0, 1),
                                param3 = c(-.1, .1), param4 = c(1e-4, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  col_idx <- sample(1:d, ceiling(max(1, param1*d)))
  row_idx <- sample(1:n, ceiling(max(2, param2*n)))

  num_val <- length(col_idx)*length(row_idx)

  dat$mat[row_idx, col_idx] <- dat$mat[row_idx, col_idx] +
    stats::rnorm(num_val, param3, param4)

  dat
}

#' Synthetic generator function: Shuffle data
#'
#' @param dat data object
#' @param param1 parameter for percentage of columns
#' @param param2 parameter for percentage of rows
#' @param ... not used
#'
#' @return data object
#' @export
generator_shuffle <- function(dat, param1 = c(0, 1), param2 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  col_idx <- sample(1:d, ceiling(max(1, param1*d)))
  row_idx <- sample(1:n, ceiling(max(2, param2*n)))

  for(i in col_idx){
    dat$mat[row_idx,i] <- sample(dat$mat[row_idx,i])
  }

  dat
}

#' Synthetic generator function: Make data independent
#'
#' Sample from the empirical distribution and add some noise
#'
#' @param dat data object
#' @param param1 parameter for percentage of columns
#' @param param2 parameter for percentage of rows
#' @param ... not used
#'
#' @return data object
#' @export
generator_decouple_empirical <- function(dat, param1 = c(0, 1), param2 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  col_idx <- sample(1:d, ceiling(max(1, param1*d)))
  row_idx <- sample(1:n, ceiling(max(2, param2*n)))

  for(i in col_idx){
    noise_lvl <- max(diff(range(dat$mat[,i]))/5, 0.1)
    dat$mat[row_idx,i] <- sample(dat$mat[,i], length(row_idx)) +
      stats::rnorm(length(row_idx), 0, noise_lvl)
  }

  dat
}

#' Synthetic generator function: Create outliers
#'
#' @param dat data object
#' @param param1 parameter for number of points
#' @param param2 parameter for expansion
#' @param ... not used
#'
#' @return data object
#' @export
generator_outlier <- function(dat, param1 = c(1, 5), param2 = c(1, 3), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  param1 <- min(floor(param1), n)
  dis_mat <- as.matrix(stats::dist(dat$mat)); diag(dis_mat) <- Inf
  vec <- apply(dis_mat, 1, min)
  row_idx <- order(vec, decreasing = T)[1:param1]

  for(i in row_idx){
    neighbor <- which.min(dis_mat[i,])
    dis <- dat$mat[i,] - dat$mat[neighbor,]
    dat$mat[i,] <- dat$mat[neighbor,] + param2*dis
  }

  dat
}
