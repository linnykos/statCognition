#' Synthetic generator function: Resample phenotype rows
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
  idx <- sample(1:n, ceiling(max(2, param1*n)))

  dat$pheno[idx,] <- dat$pheno[sample(1:n, length(idx), replace = F),]
  dat
}

#' Synthetic generator function: Shrink towards linear regression fit for phenotype
#'
#' Pick one variable in mat, and one continuous variable in pheno
#'
#' @param dat data object
#' @param param1 parameter for percentage of rows to refit
#' @param param2 parameter for shrinkage
#' @param ... not used
#'
#' @return data object
#' @export
generator_linearize_pheno <- function(dat, param1 = c(0, 1), param2 = c(0, 1), ...){
  stopifnot("mat" %in% names(dat))

  n <- nrow(dat$mat); d <- ncol(dat$mat)
  idx_mat <- sample(1:d, 1)
  row_idx <- sample(1:n, ceiling(max(2, param1*n)))

  factor_idx <- unique(c(.identify_factors(dat$pheno), which(sapply(dat$pheno, is.factor))))
  if(length(factor_idx) > 1) {
    cont_idx <- c(1:ncol(dat$pheno))[-factor_idx]
  } else cont_idx <- 1:ncol(dat$pheno)
  idx_pheno <- sample(cont_idx, 1)

  df <- data.frame(x = dat$mat[,idx_mat], y = dat$pheno[,idx_pheno])
  res <- stats::lm(y ~ x, data = df)
  df2 <- data.frame(x =  dat$mat[row_idx,idx_mat])
  pred_y <- stats::predict(res, df2)

  dis <- dat$pheno[row_idx, idx_pheno] - pred_y
  dat$pheno[row_idx, idx_pheno] <- pred_y + param2 * dis

  dat
}
