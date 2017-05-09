#' State feature: monotonic pairs
#'
#' For each pair of variables, count the median number of times the
#' sign changes when using fused lasso
#'
#' @param dat data object
#' @param num_pairs maximum number of pairs to look at
#' @param ... not used
#'
#' @return value
#' @export
state_monotone <- function(dat, num_pairs = 5, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data")

  d <- ncol(dat$mat); pairs <- .generate_pairs(d, num_pairs)

  vec <- apply(pairs, 2, function(x){
    res <- .reorder_vector(dat$mat[,x[1]], dat$mat[,x[2]])
    coef_vec <- .fused_lasso_cv(x = res$vec1, y = res$vec2)
    .alternating_direction_sum(coef_vec)/diff(range(coef_vec))
  })

  stats::median(vec)
}

#' State feature: proportion of samples remaining
#'
#' @param dat data object
#' @param prev_dat previous data object
#' @param ... not used
#'
#' @return value
#' @export
state_samples <- function(dat, prev_dat, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data")
  stopifnot("mat" %in% names(prev_dat), class(prev_dat) == "data")

  nrow(dat$mat)/nrow(prev_dat$mat)
}

#' State feature: Pairwise linearity
#'
#' @param dat data object
#' @param test_prop proportion of values left as test sample
#' @param quant quantile of the difference in residuals
#' @param num_pairs maximum number of pairs to look at
#' @param ... not used
#'
#' @return value
#' @export
state_linearity <- function(dat, test_prop = 0.1, quant = 0.75, num_pairs = 50, ...){
  stopifnot("mat" %in% names(dat), class(dat) == "data")
  n <- nrow(dat$mat); d <- ncol(dat$mat); pairs <- .generate_pairs(d, num_pairs)

  test_idx <- sample(1:n, round(test_prop*n))
  train_dat <- .remove_idx(dat, test_idx); test_dat <- .remove_idx(dat, c(1:n)[-test_idx])

  vec <- sapply(1:ncol(pairs), function(x){
    rf_resid <- .predictive_error_random_forest(train_dat$mat[,pairs[1,x]], data.frame(V1 = train_dat$mat[,pairs[2,x]]),
                                                test_dat$mat[,pairs[1,x]], data.frame(V1 = test_dat$mat[,pairs[2,x]]))

    lm_resid <- .predictive_error_linear_regression(train_dat$mat[,pairs[1,x]], data.frame(V1 = train_dat$mat[,pairs[2,x]]),
                                                    test_dat$mat[,pairs[1,x]], data.frame(V1 = test_dat$mat[,pairs[2,x]]))

    .l2norm(lm_resid)/.l2norm(rf_resid)
  })

  as.numeric(stats::quantile(vec, prob = quant))
}

###################
.fused_lasso_cv <- function(x, y){
  fit <- genlasso::fusedlasso1d(y = y, pos = x)
  lambda <- .trend_filter_minlamda(fit)

  coef_vec <- as.numeric(stats::coef(fit, lambda = lambda)$beta)
}

#reorder vec1 to be sorted and vec2 to maintain same relative position
.reorder_vector <- function(vec1, vec2){
  ord <- order(vec1, decreasing = F)
  vec1 <- vec1[ord]; vec2 <- vec2[ord]
  list(vec1 = vec1, vec2 = vec2)
}

#wrapper to ignore annoying cat statements
.trend_filter_minlamda <- function(fit){
  cv <- utils::capture.output(genlasso::cv.trendfilter(fit, verbose = F))
  idx <- grep("lambda.1se", cv)
  as.numeric(strsplit(cv[idx+1], " ")[[1]][2])
}

#adds the magnitude of jumps in opposite directions the sign changed
.alternating_direction_sum <- function(vec, tol = 1e-5){
  dif <- diff(vec)
  dif[abs(dif) < tol] <- 0
  dif <- dif[which(dif != 0)]
  if(length(dif) <= 1) return(0)
  res <- sapply(2:length(dif), function(x){
    ifelse(abs(sign(dif[x-1]) - sign(dif[x])) == 2, abs(dif[x]), 0)
  })
  sum(res)
}

.generate_pairs <- function(d, pairs){
  pairs_mat <- utils::combn(d, 2)
  pairs_mat[,1:min(ncol(pairs_mat), pairs)]
}
