state_data_monotonicity_pairs <- function(mat, num_pairs = 50, num_breakpoints = 5, ...){
  mat <- MV_remove(mat)$mat; d <- ncol(mat); pairs <- .generate_pairs(d, num_pairs)

  vec <- apply(pairs, 2, function(x){
    ord <- order(mat[,x[2]], decreasing = F)
    x1 <- mat[,x[1]][ord]; x2 <- mat[,x[2]][ord]
    fit <- genlasso::fusedlasso1d(y = x1, pos = x2)
    coef_vec <- as.numeric(stats::coef(fit, df = num_breakpoints)$beta)
    .alternating_direction_sum(coef_vec)
  })

  stats::median(vec)
}

.alternating_direction_sum <- function(vec, tol = 1e-5){
  dif <- diff(vec)
  dif[abs(dif) < tol] <- 0
  res <- sapply(2:length(dif), function(x){
    ifelse(abs(sign(dif[x-1]) - sign(dif[x])) == 2, dif[x], 0)
  })
  sum(res)
}

