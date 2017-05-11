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
  row_idx <- sample(1:n, ceiling(max(2, param1*n)))
  n2 <- length(row_idx)

  vec1 <- dat$mat[row_idx, pair[1]]; vec2 <- dat$mat[row_idx, pair[2]]
  vec1 <- sort(vec1, decreasing = bool1)
  vec2 <- sort(vec2, decreasing = bool2)

  #locally shuffle
  dist <- max(2, ceiling(param2*n2))
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
generator_inflate_correlation <- function(dat, param1 = c(0, 1), param2 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat); param1 <- max(ceiling(param1), 1)
  idx_pairs <- matrix(sample(1:d, 2*param1, replace = T), param1, 2)
  row_idx <- sample(1:n, ceiling(max(2, param1*n)))

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
generator_cluster <- function(dat, param1 = c(0, 1), param2 = c(0, 1),
                              param3 = c(0, 10), param4 = c(0, .5), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  col_idx <- sample(1:d, ceiling(max(1, param1*d)))
  row_idx <- sample(1:n, ceiling(max(2, param2*n)))
  param3 <- min(max(2, ceiling(param3)), length(row_idx) - 1)

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
generator_brownian <- function(dat, param1 = c(0, .5), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  pair <- sample(1:d, 2); d1 <- pair[1]; d2 <- pair[2]

  x <- dat$mat[,d1]; y <- dat$mat[,d2]
  brownian <- cumsum(stats::rnorm(n))
  brownian <- (brownian - min(brownian))/(max(brownian) - min(brownian))
  brownian <- (brownian * diff(range(y))) + min(y)

  assignment <- ceiling(n*(x - min(x))/diff(range(x)))
  assignment[assignment == 0] <- 1

  for(i in 1:n){
    dis <- y[i] - brownian[assignment[i]]
    y[i] <- brownian[assignment[i]] + param1 * dis
  }

  dat$mat[,d2] <- y

  dat
}

#' Synthetic generator function: Polynomial regression on a pair of variables
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows
#' @param param2 parameter for degree of polynomial
#' @param param3 parameter for shrinkage
#' @param ... not used
#'
#' @return data object
#' @export
generator_polynomial <- function(dat, param1 = c(0, 1), param2 = c(3, 6),
                                 param3 = c(0, 0.5), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  pair <- sample(1:d, 2); d1 <- pair[1]; d2 <- pair[2]
  row_idx <- sample(1:n, ceiling(max(3, param1*n)))
  anchor <- sample(1:n, ceiling(min(param2 * 3, n)))
  param2 <- max(min(round(length(anchor)/3), round(length(row_idx)/3)), 2)

  #fit the polynomial
  y <- dat$mat[anchor,d1]; x <- dat$mat[anchor,d2]; df <- data.frame(x = x, y = y)
  reg <- stats::lm(y ~ stats::poly(x, param2), data = df)

  #do prediction
  z <- data.frame(x = dat$mat[row_idx,d2])
  pred_y <- stats::predict(reg, z)

  dis <- dat$mat[row_idx,d1] - pred_y
  dat$mat[row_idx,d1] <- pred_y + param3*dis

  dat
}

#' Synthetic generator function: Shrink points around a circle for a pair of points
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows
#' @param param2 parameter for percentage width of circle
#' @param param3 parameter for percentage height of circle
#' @param param4 parameter for shrinkage
#' @param param5 parameter for start of radians
#' @param param6 parameter for number of radians
#' @param param7 parameter for angle of circle
#' @param ... not used
#'
#' @return data object
#' @export
generator_circle <- function(dat, param1 = c(0, 1), param2 = c(0, 1),
                                 param3 = c(0, 1), param4 = c(0, 0.5),
                                 param5 = c(0, 6.28), param6 = c(0, 6.28),
                                 param7 = c(0, 1.57), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  pair <- sample(1:d, 2); d1 <- pair[1]; d2 <- pair[2]
  row_idx <- sample(1:n, ceiling(max(3, param1*n)))

  #normalize the data
  x <- dat$mat[,d1]; y <- dat$mat[,d2]
  x <- (x-min(x))/diff(range(x)); y <- (y-min(y))/diff(range(y))

  #form circle
  idx <- sample(1:n, 1); center <- c(x[idx], y[idx])
  radius1 <- param2*min(center[1], 1-center[1]); radius2 <- param3*min(center[2], 1-center[2])

  #draw circle
  idx <- seq(param5, param5 + param6, length.out = 100)
  tmp1 <- (sin(idx)+1)/2*radius1+center[1]; tmp2 <- (cos(idx)+1)/2*radius2+center[2]

  #rotate circle
  circle <- cbind(tmp1, tmp2)
  rot_mat <- matrix(c(cos(param7), sin(param7), -sin(param7), cos(param7)), 2, 2)
  circle <- circle %*% rot_mat

  #shrink points towards the circle
  for(i in row_idx){
    dis <- apply(circle, 1, function(z){
      .l2norm(z - c(x[i], y[i]))
    })
    idx <- which.min(dis)
    vec <- c(x[i], y[i]) - circle[idx,]
    x[i] <- circle[idx,1] + param4 * vec[1]
    y[i] <- circle[idx,2] + param4 * vec[2]
  }

  #rescale x and y
  x <- x*diff(range(dat$mat[,d1])) + min(dat$mat[,d1])
  y <- y*diff(range(dat$mat[,d2])) + min(dat$mat[,d2])

  dat$mat[,pair] <- cbind(x,y)

  dat
}

