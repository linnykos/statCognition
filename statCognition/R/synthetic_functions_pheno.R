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
  idx <- sample(1:n, ceiling(max(2, param1*n)))

  dat$pheno[idx,] <- dat$pheno[sample(1:n, length(idx), replace = F),]
  dat
}
