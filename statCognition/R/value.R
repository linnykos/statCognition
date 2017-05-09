value <- function(surface){
  res <- structure(list(surface = surface), class = "value")

  is_valid(res)
  res
}

is_valid.value <- function(obj, ...){
  stopifnot(all(names(obj) == "surface"))
  stopifnot(all(names(obj$surface) == c("hash", "table")))

  stopifnot(class(obj$surface$hash) == "hash")
  stopifnot(all(sapply(obj$surface$table, class) == "numeric"))
  stopifnot(all(sapply(obj$surface$table, is.matrix) == FALSE))

  TRUE
}

