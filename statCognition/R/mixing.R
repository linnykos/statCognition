generate_synthetic_data <- function(mat, mixing_list = generate_mixing_default(),
                        distortion_param = 0.4, stopping_param = 0.8, base.seed = 10, ...){

  set.seed(base.seed)
  while(TRUE){
    idx_row <- sample(1:nrow(mat), round(distortion_param * stats::runif(1) * nrow(mat)))
    mat[idx_row,] <- mixing_list[[sample(1:length(mixing_list), 1)]](mat[idx_row,], ...)

    if(stats::runif(1) >= stopping_param) break()
  }

  mat
}

generate_mixing_default <- function(package_name = "statCognition",
                                 function_starter = "mixing"){
  if(!isNamespaceLoaded(package_name)) stop(paste(package_name, "is not",
                                                  "currently loaded"))

  all.obj <- ls(paste0("package:", package_name))
  print(all.obj)

  print("YOLO")
  fun <- grep(paste0("^", function_starter, "_*"), all.obj, value = T)
  print(fun)

  lis <- lapply(fun, function(x){
    eval(parse(text = paste0(package_name, "::", x)))
  })
  names(lis) <- fun

  structure(lis, class = "mixing_list")
}
