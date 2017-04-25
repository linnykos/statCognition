state_data_residual_RF_LR <- function(mat, test_prop = 0.1, quant = 0.75, ...){
  mat <- MV_remove(mat)$mat
  n <- nrow(mat); d <- ncol(mat)

  mat <- as.data.frame(mat)
  test_idx <- sample(1:n, round(test_prop*n))
  train_mat <- mat[-test_idx,,drop = F]; test_mat <- mat[test_idx,,drop = F]

  vec <- sapply(1:d, function(x){
    rf_fit <- randomForest::randomForest(x = train_mat[,-x], y = train_mat[,x])
    rf_resid <- stats::predict(rf_fit, test_mat[,-x]) - test_mat[,x]

    lm_fit <- stats::lm(eval(parse(text = paste0(colnames(mat)[x],"~."))), data = train_mat)
    lm_resid <- stats::predict(lm_fit, test_mat[,-x]) - test_mat[,x]

  .l2norm(rf_resid - lm_resid)
  })

  as.numeric(stats::quantile(vec, prob = quant))
}

.l2norm <- function(x){
  sqrt(sum(x^2))
}
