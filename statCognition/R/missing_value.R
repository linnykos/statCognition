MV_remove <- function(mat, ...){
  idx <- apply(mat, 1, function(x){ifelse(any(is.na(x)), FALSE, TRUE)})
  mat[idx,,drop = F]
}

MV_maximum_likelihood <- function(mat, shrinkage = 0.1, ...){
  idx <- apply(mat, 1, function(x){ifelse(any(is.na(x)), FALSE, TRUE)})
  mu <- colMeans(mat[idx,])
  Sigma <- stats::cov(mat[idx,])
  Sigma <- Sigma + shrinkage*diag(ncol(mat))

  t(apply(mat, 1, function(x){
    bool <- is.na(x)
    if(all(!bool)) return(x)

    idx_missing <- which(bool == TRUE); idx_known <- c(1:length(x))[-idx_missing]
    x[idx_missing] <- mu[idx_missing] + Sigma[idx_missing, idx_known] %*% solve(Sigma[idx_known, idx_known]) %*%
      (x[idx_known] - mu[idx_known])
    x
  }))
}

MV_mean <- function(mat, ...){
  apply(mat, 2, function(x){
    mu <- mean(x, na.rm = T)
    bool <- is.na(x)
    x[bool] <- mu
    x
  })
}

MV_matrix_completion <- function(mat, rank = ceiling(ncol(mat)/2), lambda = 1, ...){
  res <- softImpute::softImpute(mat, rank.max = rank, lambda = lambda, ...)
  res$u %*% diag(res$d) %*% t(res$v)
}
