data_object <- function(lis){
  structure(lis, class = "data")
}

get_primary <- function(dat){
  stopifnot(class(dat) == "data")
  dat[[1]]
}
