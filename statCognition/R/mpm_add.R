.add_mpm <- function(obj1, obj2, weight1 = 1, weight2 = 1){
  stopifnot(class(obj1) == "mpm", class(obj2) == "mpm")

  #sort the two mpm's
  res <- .order_vectors(obj1, obj2)
  vec1 <- weight1 * res$vec1; vec2 <- weight2 * res$vec2

  #fill in 0's
  vec1 <- .fill_zeros(vec1); vec2 <- .fill_zeros(vec2)

  .monotone_piecewise_marginal(sort(c(obj1$breakpoints, obj2$breakpoints)),
                               vec1 + vec2)
}

.order_vectors <- function(obj1, obj2){
  breakpoints <- sort(c(obj1$breakpoints, obj2$breakpoints))

  vec1 <- rep(0, length(breakpoints)); vec2 <- rep(0, length(breakpoints))

  idx1 <- intersect(which(breakpoints %in% obj1$breakpoints), which(!duplicated(breakpoints)))
  vec1[idx1] <- obj1$values
  vec2[-idx1] <- obj2$values

  list(vec1 = vec1, vec2 = vec2)
}

.fill_zeros <- function(vec){
  tmp <- vec[1]
  for(i in 2:length(vec)){
    if(vec[i] == 0) {
      vec[i] <- tmp
    } else {
      tmp <- vec[i]
    }
  }

  vec
}
