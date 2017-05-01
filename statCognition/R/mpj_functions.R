.add_mpj <- function(obj1, obj2, weight1 = 1, weight2 = 1){
  stopifnot(class(obj1) == "mpj", class(obj2) == "mpj",
            length(obj1) == length(obj2), all(names(obj1) == names(obj2)))

  n <- length(obj1)
  lis <- lapply(1:n, function(x){.add_mpm(obj1[[x]], obj2[[x]], weight1 = weight1,
                                          weight2 = weight2)})
  names(lis) <- names(obj1)

  .mpj_from_mpm(lis)
}

.estimate_mpj <- function(){

}

.evaluate_mpj <- function(){

}

.plot_mpj <- function(){

}
