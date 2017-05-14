data_object <- function(lis){
  structure(lis, class = "data")
}

get_primary <- function(dat){
  stopifnot(class(dat) == "data")
  dat[[1]]
}

#' Checks data object for validity
#'
#' Runs the functions on the minimum and maximum values on the dataset
#'
#' @param obj The object to check
#' @param ... not used
#'
#' @return boolean
#' @export
is_valid.data <- function(obj, ...){
  if("synthetic_seed" %in% names(obj)){
    stopifnot(is.numeric(obj$synthetic_seed), length(obj$synthetic_seed) == 1,
              obj$synthetic_seed %% 1 == 0)
  }

  TRUE
}

.remove_idx <- function(dat, idx){
  if("mat" %in% names(dat)) dat$mat <- dat$mat[-idx,,drop = F]
  if("pheno" %in% names(dat)) dat$pheno <- dat$pheno[-idx,,drop = F]
  dat
}
