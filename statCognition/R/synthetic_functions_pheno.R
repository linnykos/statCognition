#' Synthetic generator function: Resample phenotype rows
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows to refit
#' @param ... not used
#'
#' @return data object
#' @export
generator_resample_pheno <- function(dat, param1 = c(0, 1), ...){
  stopifnot(all(c("mat", "pheno") %in% names(dat)))

  n <- nrow(dat$pheno)
  idx <- sample(1:n, ceiling(max(2, param1*n)))

  dat$pheno[idx,] <- dat$pheno[sample(1:n, length(idx), replace = F),]
  dat
}

#' Synthetic generator function: Shrink towards linear regression fit for phenotype
#'
#' Pick one variable in mat, and one continuous variable in pheno
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows
#' @param param2 parameter for shrinkage
#' @param ... not used
#'
#' @return data object
#' @export
generator_linearize_pheno <- function(dat, param1 = c(0, 1), param2 = c(0, 1), ...){
  stopifnot(all(c("mat", "pheno") %in% names(dat)))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  idx_mat <- sample(1:d, 1)
  row_idx <- sample(1:n, ceiling(max(2, param1*n)))
  idx_pheno <- .select_continuous_idx(dat$pheno)

  df <- data.frame(x = dat$mat[,idx_mat], y = dat$pheno[,idx_pheno])
  res <- stats::lm(y ~ x, data = df)
  df2 <- data.frame(x =  dat$mat[row_idx,idx_mat])
  pred_y <- stats::predict(res, df2)

  dis <- dat$pheno[row_idx, idx_pheno] - pred_y
  dat$pheno[row_idx, idx_pheno] <- pred_y + param2 * dis

  dat
}

#' Synthetic generator function: Sort pheno in order with respect to one variable
#'
#' Pick one variable in mat, and one continuous variable in pheno
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows
#' @param param2 parameter for strength of monotonicity
#' @param ... not used
#'
#' @return data object
#' @export
generator_monotonic_pheno <- function(dat, param1 = c(0, 1), param2 = c(0, 1), ...){
  stopifnot(all(c("mat", "pheno") %in% names(dat)))

  bool1 <- sample(c(TRUE, FALSE), 1)

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  idx_mat <- sample(1:d, 1)
  row_idx <- sample(1:n, ceiling(max(2, param1*n)))
  idx_pheno <- .select_continuous_idx(dat$pheno)
  n2 <- length(row_idx)

  vec1 <- dat$mat[row_idx, idx_mat]; vec2 <- dat$pheno[row_idx, idx_pheno]
  vec2 <- sort(vec2, decreasing = bool1)

  #locally shuffle
  dist <- max(2, ceiling(param2*n2))
  for(i in 1:max(n2 - dist, 1)){
    vec2[i:(i+dist-1)] <- sample(vec2[i:(i+dist-1)])
  }

  vec2 <- vec2[rank(vec1)]

  dat$pheno[row_idx,idx_pheno] <- vec2

  dat
}

#' Synthetic generator function: Cluster phenotype and variable
#'
#' Pick one variable in mat, and one continuous variable in pheno
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows
#' @param param2 parameter for percentage of columns
#' @param param3 parameter for number of clusters
#' @param ... not used
#'
#' @return data object
#' @export
generator_cluster_pheno <- function(dat, param1 = c(0, 1), param2 = c(0, 1),
                                    param3 = c(2, 10), ...){
  stopifnot(all(c("mat", "pheno") %in% names(dat)))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  row_idx <- sample(1:n, ceiling(max(3, param1*n)))
  col_idx <- sample(1:d, ceiling(max(2, param2*d)))
  idx_pheno <- .select_continuous_idx(dat$pheno)
  n2 <- length(row_idx)
  param3 <- min(n2-1, round(param3))

  #cluster
  res1 <- suppressWarnings(stats::kmeans(dat$pheno[row_idx, idx_pheno], param3))
  res2 <- suppressWarnings(stats::kmeans(dat$mat[row_idx, col_idx], param3))

  pheno <- dat$pheno
  #reassign
  for(i in 1:param3){
    idx1 <- which(res1$cluster == i); idx2 <- which(res2$cluster == i)
    pheno[row_idx[idx2], idx_pheno] <- dat$pheno[row_idx[sample(idx1, length(idx2), replace = T)], idx_pheno]
  }

  dat$pheno <- pheno

  dat
}

.select_continuous_idx <- function(pheno){
  factor_idx <- unique(c(.identify_factors(pheno), which(sapply(pheno, is.factor))))
  if(length(factor_idx) > 1) {
    cont_idx <- c(1:ncol(pheno))[-factor_idx]
  } else cont_idx <- 1:ncol(pheno)
  sample(cont_idx, 1)
}
