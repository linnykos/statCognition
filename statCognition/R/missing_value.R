#' Missing value treatment via removal
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
MV_remove <- function(mat, pheno = NA, ...){
  idx <- apply(mat, 1, function(x){ifelse(any(is.na(x)), FALSE, TRUE)})
  mat <- mat[idx,,drop = F]

  if(!any(is.na(pheno))){
    pheno <- pheno[idx,,drop = F]
  }
  list(mat = mat, pheno = pheno)
}

#' Missing value treatment via maximum likelihood
#'
#' @param mat matrix
#' @param pheno data frame
#' @param shrinkage amount of regularization to covariance matrix
#' @param ... not used
#'
#' @return matrix
#' @export
MV_maximum_likelihood <- function(mat, pheno = NA, shrinkage = 0.1, ...){
  idx <- apply(mat, 1, function(x){ifelse(any(is.na(x)), FALSE, TRUE)})
  mu <- colMeans(mat[idx,])
  Sigma <- stats::cov(mat[idx,])
  Sigma <- Sigma + shrinkage*diag(ncol(mat))

  mat <- t(apply(mat, 1, function(x){
    bool <- is.na(x)
    if(all(!bool)) return(x)

    idx_missing <- which(bool == TRUE); idx_known <- c(1:length(x))[-idx_missing]
    x[idx_missing] <- mu[idx_missing] + Sigma[idx_missing, idx_known] %*% solve(Sigma[idx_known, idx_known]) %*%
      (x[idx_known] - mu[idx_known])
    x
  }))

  list(mat = mat, pheno = pheno)
}

#' Missing value treatment via matching
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
MV_matching <- function(mat, pheno = NA, ...){
  mat <- t(apply(mat, 1, function(x){
    idx <- which(is.na(x))
    if(length(idx) == 0) return(x)

    elgible_rows <- which(apply(mat, 1, function(y){
      ifelse(!any(is.na(y[idx])), TRUE, FALSE)
    }))

    vec <- sapply(elgible_rows, function(y){
      .l2norm(y[-idx] - x[-idx])
    })

    selected_row <- elgible_rows[which.min(vec)]
    x[idx] <- mat[selected_row, idx]
    x
  }))

  list(mat = mat, pheno = pheno)
}

#' Missing value treatment via matrix completion
#'
#' Uses softimpute
#'
#' @param mat matrix
#' @param pheno data frame
#' @param rank rank of the latent factors
#' @param lambda tuning parameter
#' @param ... not used
#'
#' @return matrix
#' @export
MV_matrix_completion <- function(mat, pheno = NA, rank = ceiling(ncol(mat)/2), lambda = 1, ...){
  res <- softImpute::softImpute(mat, rank.max = rank, lambda = lambda, ...)
  mat <- res$u %*% diag(res$d) %*% t(res$v)

  list(mat = mat, pheno = pheno)
}
