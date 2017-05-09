#' State feature: Phenotype residuals
#'
#' @param dat data object
#' @param test_prop proportion of values left as test sample
#' @param quant quantile of the difference in residuals
#' @param ... not used
#'
#' @return value
#' @export
state_pheno_residual <- function(dat, test_prop = 0.1, quant = 0.75, ...){
  n <- nrow(dat$mat); d <- ncol(dat$mat)

  pheno_mod <- .adjust_data_frame_regression(dat$pheno)

  test_idx <- sample(1:n, round(test_prop*n))
  train_dat <- .remove_idx(dat, test_idx); test_dat <- .remove_idx(dat, c(1:n)[-test_idx])
  train_pheno_mod <- pheno_mod[-test_idx,,drop = F]; test_pheno_mod <- pheno_mod[test_idx,,drop = F]

  vec <- sapply(1:d, function(x){
    rf_resid <- .predictive_error_random_forest(train_dat$mat[,x], train_dat$pheno,
                                                test_dat$mat[,x], test_dat$pheno)

    lm_resid <- .predictive_error_linear_regression(train_dat$mat[,x], train_pheno_mod,
                                                    test_dat$mat[,x], test_pheno_mod)

    .l2norm(lm_resid)/.l2norm(rf_resid)
  })

  as.numeric(stats::quantile(vec, prob = quant))
}

#' State feature: Phenotype mutual information
#'
#' @param dat data object
#' @param numBins number of discretized bins
#' @param ... not used
#'
#' @return value
#' @export
state_pheno_MI <- function(dat, numBins = 4, ...){
  stopifnot(c("mat", "pheno") %in% names(dat), class(dat) == "data")

  n <- nrow(dat$mat); d <- ncol(dat$mat)

  pairs <- expand.grid(1:d, 1:ncol(dat$pheno))
  vec <- apply(pairs, 1, function(x){
    bins <- .discretize_bins(dat$mat[,as.numeric(x[1])], dat$pheno[,as.numeric(x[2])], numBins)
    entropy::mi.empirical(bins)
  })

  stats::median(vec)
}

####################

.predictive_error_random_forest <- function(train_response, train_dat, test_reponse, test_dat){
  fit <- randomForest::randomForest(x = train_dat, y = train_response)
  res <- stats::predict(fit, test_dat) - test_reponse
}

.predictive_error_linear_regression <- function(train_response, train_dat, test_reponse, test_dat){
  lm_fit <- stats::lm(train_response~., data = train_dat)
  lm_resid <- stats::predict(lm_fit, test_dat) - test_reponse
}

.discretize_bins <- function(mat_vec, pheno_vec, numBins){
  if(is.factor(pheno_vec)){
    table(.cut_sequence_bins(mat_vec, numBins = numBins), pheno_vec)
  } else {
    entropy::discretize2d(mat_vec, pheno_vec, numBins1 = numBins, numBins2 = numBins)
  }
}

## taken from entropy::discretize
.cut_sequence_bins <- function(vec, numBins, r = range(vec)){
  b <- seq(from = r[1], to = r[2], length.out = numBins + 1)
  cut(vec, breaks = b, include.lowest = TRUE)
}

.l2norm <- function(x){
  sqrt(sum(x^2))
}
