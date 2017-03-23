MV_remove <- function(mat, ...){
  idx <- apply(mat, 1, function(x){ifelse(any(is.na(x)), FALSE, TRUE)})
  mat[idx,]
}

MV_maximum_likelihood <- function(mat, ...){
  idx <- apply(mat, 1, function(x){ifelse(any(is.na(x)), FALSE, TRUE)})
  mu <- colMeans(mat[idx,])
  Sigma <- stats::cov(mat[idx,])

  mat <- t(apply(mat, 1, function(x){
    bool <- is.na(x)
    if(all(!bool)) return(x)

    idx_missing <- which(bool == TRUE); idx_known <- c(1:length(x))[-idx_missing]
    mu[idx_missing] + Sigma[idx_missing, idx_known] %*% solve(Sigma[idx_known, idx_known]) %*%
      (x[idx_known] - mu[idx_known])
  }))
}

MV_mean <- function(mat, ...){
  apply(mat, 2, function(x){
    mu <- mean(x, na.rm = T)
    bool <- is.na(x)
    x[bool] <- mu
  })
}

MV_matrix_completion <- function(mat, rank = ceiling(ncol(mat)/2), lambda = 10, ...){
  softImpute::softImpute(mat, rank.max = rank, lambda = lambda, ...)
}
