data_object <- function(lis){
  structure(lis, class = "data")
}

get_primary <- function(dat){
  stopifnot(class(dat) == "data")
  dat[[1]]
}

.remove_idx <- function(dat, idx){
  if("mat" %in% names(dat)) dat$mat <- dat$mat[-idx,,drop = F]
  if("pheno" %in% names(dat)) dat$pheno <- dat$pheno[-idx,,drop = F]
  dat
}
