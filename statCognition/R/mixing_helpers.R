.mixing_shuffle_columns <- function(mat, percent_columns = 0.1){
  stopifnot(percentage_columns <= 1)

  idx_columns <- sample(1:ncol(mat), round(percentage_columns * ncol(mat)))
  mat[,idx_columns] <- mat[,sample(idx_columns)]

  mat
}

.mixing_shuffle_rows <- function(mat, percent_columns = 0.1){
  stopifnot(percentage_columns <= 1)

  idx_columns <- sample(1:ncol(mat), round(percentage_columns * ncol(mat)))
  mat[,idx_columns] <- mat[sample(1:nrow(mat)), idx_columns]

  mat
}

.mixing_refit_normality <- function(mat){
  mu <- colMeans(mat)
  Sigma <- stats::cov(mat)

  MASS::mvrnorm(nrow(mat), mu, Sigma)
}

.mixing_resample <- function(mat){
  idx <- sample(1:nrow(mat), nrow(mat), replace = T)
  mat[idx,]
}

.mixing_swap_rows <- function(mat, idx1, idx2){
  stopifnot(length(idx1) == length(idx2), length(intersect(idx1, idx2)) == 0)
  mat[sample(idx1),] <- tmp; mat[sample(idx2),] <- tmp2
  mat[idx1,] <- tmp2; mat[idx2,] <- tmp

  mat
}

.mixing_adjust_mean <- function(mat, noise_func = function(x){x + sd(x)/2*rnorm(length(x))}){
  apply(mat, 1, function(y){noise_func(y)})
}

.mixing_shrink_correlation <- function(mat, percentage_columns = 0.1){
  stopifnot(percentage_columns <= 1, ncol(mat) >= 2)

  idx <- sample(1:ncol(mat), floor(percentage_columns*ncol(mat)/2)*2)
  if(length(idx) <= 2) idx <- sample(ncol(mat), 2)
  idx_pairs <- matrix(idx, ncol = 2, drop = F)

  for(i in 1:nrow(idx_pairs)){
    mat[,idx_pairs[i,2]] <- rowMeans(mat[,idx_pairs[i,]])
  }

  mat
}
