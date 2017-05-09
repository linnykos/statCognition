#' Removing confounders via none
#'
#' @param dat data object
#' @param ... not used
#'
#' @return matrix
#' @export
RC_none <- function(dat, ...){
  dat
}

#' Removing confounders via linear regression
#'
#' @param dat data object
#' @param ... not used
#'
#' @return matrix
#' @export
RC_linear_regression <- function(dat, ...){
  stopifnot(c("mat", "pheno") %in% names(dat), class(dat) == "data")

  pheno_mod <- .adjust_data_frame_regression(dat$pheno)
  mat <- .regress_confounder(dat$mat, pheno_mod)

  data_object(list(mat = mat))
}

#' Removing confounders via paired difference
#'
#' @param dat data object
#' @param ... not used
#'
#' @return matrix
#' @export
RC_pairing_difference <- function(dat, ...){
  stopifnot(c("mat", "pheno") %in% names(dat), class(dat) == "data")

  pheno_mod <- as.matrix(.adjust_data_frame_regression(dat$pheno))
  dis <- as.matrix(stats::dist(scale(pheno_mod)))
  pairings <- .construct_pairings_confounding(dis)

  mat <- .compute_difference_pairings(dat$mat, pairings)

  data_object(list(mat = mat))
}

######
#columns with only 2 variables are factors
.identify_factors <- function(pheno){
  as.numeric(which(apply(pheno, 2, function(x){length(unique(x))}) == 2))
}

.adjust_data_frame_regression <- function(pheno){
  idx <- unique(c(.identify_factors(pheno), which(sapply(pheno, is.factor))))
  if(length(idx) > 0){
    pheno_mod <- as.matrix(pheno[,-idx,drop = F])

    pheno_mod <- cbind(pheno_mod, .split_factors(pheno, idx))
  } else {
    pheno_mod <- pheno
  }

  as.data.frame(pheno_mod)
}

.split_factors <- function(pheno, idx){
  mat_all <- numeric(0)
  for(i in idx){
    lev <- levels(as.factor(as.character(pheno[,i])))
    mat <- sapply(1:(length(lev)-1), function(x){
      as.numeric(pheno[,i] == lev[x])
    })

    mat_all <- cbind(mat_all, mat)
  }

  as.data.frame(mat_all)
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
