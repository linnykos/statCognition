#' Removing confounders via none
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
RC_none <- function(mat, pheno, ...){
  stopifnot(nrow(mat) == nrow(pheno), is.matrix(mat), is.data.frame(pheno))

  mat
}

#' Removing confounders via linear regression
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
RC_linear_regression <- function(mat, pheno, ...){
  stopifnot(nrow(mat) == nrow(pheno), is.matrix(mat), is.data.frame(pheno))

  pheno_mod <- .adjust_data_frame_regression(pheno)
  .regress_confounder(mat, pheno_mod)
}

#' Removing confounders via paired difference
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
RC_pairing_difference <- function(mat, pheno, ...){
  stopifnot(nrow(mat) == nrow(pheno), is.matrix(mat), is.data.frame(pheno))

  pheno_mod <- as.matrix(.adjust_data_frame_regression(pheno))
  dis <- as.matrix(stats::dist(scale(pheno_mod)))
  pairings <- .construct_pairings_confounding(dis)

  .compute_difference_pairings(mat, pairings)
}

#' Removing confounders via random forest
#'
#' @param mat matrix
#' @param pheno data frame
#' @param ... not used
#'
#' @return matrix
#' @export
RC_random_forest_regression <- function(mat, pheno, ...){
  stopifnot(nrow(mat) == nrow(pheno), is.matrix(mat), is.data.frame(pheno))

  apply(mat, 2, function(x){
    x - randomForest::randomForest(pheno, x, ...)$predicted
  })
}

######
.identify_factors <- function(dat){
  which(apply(dat, 2, function(x){length(unique(x))}) == 2)
}

.adjust_data_frame_regression <- function(dat){
  idx <- unique(.identify_factors(dat), which(sapply(dat, is.factor)))
  if(length(idx) > 0){
    dat_mod <- as.matrix(dat[,-idx,drop = F])

    dat_mod <- cbind(dat_mod, .split_factors(dat, idx))
  } else {
    dat_mod <- dat
  }

  as.data.frame(dat_mod)
}

.split_factors <- function(dat, idx){
  mat_all <- numeric(0)
  for(i in idx){
    lev <- levels(as.factor(as.character(dat[,i])))
    mat <- sapply(1:(length(lev)-1), function(x){
      as.numeric(dat[,i] == lev[x])
    })

    mat_all <- cbind(mat_all, mat)
  }

  mat_all
}

.regress_confounder <- function(mat, dat){
  apply(mat, 2, function(x){
    stats::lm(x ~ ., data = dat)$residuals
  })
}

.construct_pairings_confounding <- function(dis_mat){
  count <- floor(nrow(dis_mat)/2)
  pairings <- matrix(NA, count, 2)
  diag(dis_mat) <- Inf

  idx_vec <- 1:nrow(dis_mat)

  for(i in 1:count){
    min_val <- min(dis_mat)
    idx <- which(dis_mat == min_val, arr.ind = T)[1,]
    pairings[i,] <- idx_vec[idx]

    dis_mat <- dis_mat[-idx, -idx]
    idx_vec <- idx_vec[-idx]
  }

  pairings
}

.compute_difference_pairings <- function(mat, pairings){
  stopifnot(max(pairings) <= nrow(mat))

  t(apply(pairings, 1, function(x){
    mat[x[1],] - mat[x[2],]
  }))
}
