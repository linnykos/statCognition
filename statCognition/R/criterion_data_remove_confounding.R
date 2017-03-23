criterionData_pheno_residual_RF_LR <- function(mat, pheno, test_prop = 0.1, quant = 0.75, ...){
  res <- .MV_remove_with_pheno(mat, pheno); mat <- res$mat; pheno <- res$pheno
  n <- nrow(mat); d <- ncol(mat)

  pheno_mod <- .adjust_data_frame_regression(pheno)

  test_idx <- sample(1:n, round(test_prop*n))
  train_mat <- mat[-test_idx,,drop = F]; test_mat <- mat[test_idx,,drop = F]
  train_pheno <- pheno[-test_idx,,drop = F]; test_pheno <- pheno[test_idx,,drop = F]
  train_pheno_mod <- pheno_mod[-test_idx,,drop = F]; test_pheno_mod <- pheno_mod[test_idx,,drop = F]

  vec <- sapply(1:d, function(x){
    rf_fit <- randomForest::randomForest(x = train_pheno, y = train_mat[,x])
    rf_resid <- stats::predict(rf_fit, test_pheno) - test_mat[,x]

    y <- train_mat[,x]
    lm_fit <- stats::lm(y~., data = train_pheno_mod)
    lm_resid <- stats::predict(lm_fit, test_pheno_mod) - test_mat[,x]

    .l2norm(rf_resid - lm_resid)
  })

  as.numeric(stats::quantile(vec, prob = quant))
}

criterionData_pheno_MI <- function(mat, pheno, numBins = 4, ...){
  res <- .MV_remove_with_pheno(mat, pheno); mat <- res$mat; pheno <- res$pheno
  n <- nrow(mat); d <- ncol(mat)

  pairs <- expand.grid(1:d, 1:ncol(pheno))
  vec <- apply(pairs, 1, function(x){
    bins <- .discretize_bins(mat[,as.numeric(x[1])], pheno[,as.numeric(x[2])], numBins)
    entropy::mi.empirical(bins)
  })

  stats::median(vec)
}

##############

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

.MV_remove_with_pheno <- function(mat, pheno){
  idx <- apply(mat, 1, function(x){ifelse(any(is.na(x)), FALSE, TRUE)})
  mat <- mat[idx,,drop = F]
  pheno <- pheno[idx,,drop = F]

  list(mat = mat, pheno = pheno)
}
