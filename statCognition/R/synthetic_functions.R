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

  n <- nrow(dat$mat)
  idx <- sample(1:n, ceiling(max(1, param1*n)))

  dat$mat[idx,] <- MASS::mvrnorm(length(idx), mu, Sigma)
  dat
}

#' Synthetic generator function: Resample rows
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows to refit
#' @param ... not used
#'
#' @return data object
#' @export
generator_resample <- function(dat, param1 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat)
  idx <- sample(1:n, ceiling(max(1, param1*n)))

  dat$mat[idx,] <- dat$mat[sample(idx, replace = T),]
  dat
}

#' Synthetic generator function: Resample rows
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows to refit
#' @param ... not used
#'
#' @return data object
#' @export
generator_resample_pheno <- function(dat, param1 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$pheno)
  idx <- sample(1:n, ceiling(max(1, param1*n)))

  dat$pheno[idx,] <- dat$pheno[sample(1:n, length(idx), replace = F),]
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
                                param3 = c(-.1, .1), param4 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  col_idx <- sample(1:d, ceiling(max(1, param1*n)))
  row_idx <- sample(1:n, ceiling(max(1, param2*n)))

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
  col_idx <- sample(1:d, ceiling(max(1, param1*n)))
  row_idx <- sample(1:n, ceiling(max(1, param2*n)))

  val <- as.numeric(dat$mat[row_idx, col_idx])

  dat$mat[row_idx, col_idx] <- sample(val)

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
  col_idx <- sample(1:d, ceiling(max(1, param1*n)))
  row_idx <- sample(1:n, ceiling(max(1, param2*n)))

  for(i in col_idx){
    noise_lvl <- diff(range(dat$mat[,i]))/5
    dat$mat[row_idx,i] <- sample(dat$mat[,i], length(row_idx)) +
      stats::rnorm(length(row_idx), 0, noise_lvl)
  }

  dat
}

#' Synthetic generator function: Make a pair of variables monotonically dependent
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows
#' @param param2 parameter for strength of monotonicity
#' @param ... not used
#'
#' @return data object
#' @export
generator_monotonic <- function(dat, param1 = c(0, 1), param2 = c(0,1), ...){
  stopifnot("mat" %in% names(dat))

  bool1 <- sample(c(TRUE, FALSE), 1); bool2 <- sample(c(TRUE, FALSE), 1)

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  pair <- sample(1:d, 2)
  row_idx <- sample(1:n, ceiling(max(1, param1*n)))
  n2 <- length(row_idx)

  vec1 <- dat$mat[row_idx, pair[1]]; vec2 <- dat$mat[row_idx, pair[2]]
  vec1 <- sort(vec1, decreasing = bool1)
  vec2 <- sort(vec2, decreasing = bool2)

  #locally shuffle
  dist <- ceiling(param2*n2)
  for(i in 1:max(n2 - dist, 1)){
    vec1[i:(i+dist-1)] <- sample(vec1[i:(i+dist-1)])
    vec2[i:(i+dist-1)] <- sample(vec2[i:(i+dist-1)])
  }

  dat$mat[row_idx, pair[1]] <- vec1; dat$mat[row_idx, pair[2]] <- vec2

  dat
}

#' Synthetic generator function: Inflate the correlation between pairs of variables
#'
#' Set one variable to the mean of itself and another variable
#'
#' @param dat data object
#' @param param1 parameter for number of pairs to consider
#' @param param2 parameter for percentage of rows
#' @param ... not used
#'
#' @return data object
#' @export
generator_inflate_correlation <- function(dat, param1 = c(0, 10), param2 = c(0,1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat); param1 <- ceiling(param1)
  idx_pairs <- matrix(sample(1:d, 2*param1, replace = T), param1, 2)
  row_idx <- sample(1:n, ceiling(max(1, param1*n)))

  for(i in 1:nrow(idx_pairs)){
    dat$mat[row_idx,idx_pairs[i,2]] <- rowMeans(dat$mat[row_idx,idx_pairs[i,]])
  }

  dat
}

#' Synthetic generator function: Cluster points
#'
#' @param dat data object
#' @param param1 parameter for percentage of columns
#' @param param2 parameter for percentage of rows
#' @param param3 parameter for number of clusters
#' @param param4 parameter for shrinkage
#' @param ... not used
#'
#' @return data object
#' @export
generator_cluster <- function(dat, param1 = c(0, 1), param2 = c(0,1),
                              param3 = c(0,10), param4 = c(-1,1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  col_idx <- sample(1:d, ceiling(max(1, param1*n)))
  row_idx <- sample(1:n, ceiling(max(1, param2*n)))
  param3 <- ceiling(param3)

  res <- stats::kmeans(dat$mat[row_idx, col_idx], param3)

  #move each point to/from its cluster
  for(i in 1:length(row_idx)){
    dis <- dat$mat[row_idx[i], col_idx] - res$centers[res$cluster[i],]
    dat$mat[row_idx[i], col_idx] <- res$centers[res$cluster[i],] + param4 * dis
  }

  dat
}

#' Synthetic generator function: Adjust a pair of variables based on Brownian motion
#'
#' @param dat data object
#' @param param1 parameter for shrinkage
#' @param ... not used
#'
#' @return data object
#' @export
generator_brownian <- function(dat, param1 = c(-1, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  pair <- sample(1:d, 2); d1 <- pair[1]; d2 <- pair[2]

  x <- dat$mat[,d1]; y <- dat$mat[,d2]
  brownian <- cumsum(stats::rnorm(n))
  brownian <- (brownian - min(brownian))/(max(brownian) - min(brownian))
  brownian <- (brownian + min(y))*max(y)

  assignment <- ceiling(n*(x - min(x)/diff(range(x))))

  for(i in 1:n){
    dis <- y[i] - brownian[assignment[i]]
    y[i] <- brownian[assignment[i]] + param1 * dis
  }

  dat$mat[,d2] <- y

  dat
}

