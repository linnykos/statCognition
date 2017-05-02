.add_mpj <- function(obj1, obj2, weight1 = 1, weight2 = 1){
  stopifnot(class(obj1) == "mpj", class(obj2) == "mpj",
            length(obj1) == length(obj2), all(names(obj1) == names(obj2)))

  n <- length(obj1)
  lis <- lapply(1:n, function(x){.add_mpm(obj1[[x]], obj2[[x]], weight1 = weight1,
                                          weight2 = weight2)})
  names(lis) <- names(obj1)

  .mpj_from_mpm(lis)
}

.estimate_mpj <- function(mat_lis, idx_lis){
  stopifnot(length(mat_lis) == length(idx_lis), is.list(mat_lis), is.list(idx_lis))

  n <- length(mat_lis)
  lis <- lapply(1:n, function(x){.estimate_mpm(mat_lis[[x]], idx_lis[[x]])})
  names(lis) <- names(mat_lis)

  .mpj_from_mpm(lis)
}

.evaluate_mpj <- function(obj, vec){
  stopifnot(length(obj) == length(vec))
  n <- length(vec)

  sapply(1:n, function(x){.evaluate_mpm(obj[[x]], vec[x])})
}
