generate_synthetic_initializer <- function(mixing_list = generate_mixing_default(),
                                           distortion_param = 0.4, stopping_param = 0.8,
                                           max_iter = 10){
  structure(list(mixing_list = mixing_list, distortion_param = distortion_param,
                 stopping_param = stopping_param, max_iter = max_iter),
            class = "generator_init")
}

generate_synthetic_data <- function(mat, init, ...){
  seed <- round(stats::runif(1)*1000)

  mat_synthetic <- .generate_synthetic_data_from_seed(mat, init, base_seed = seed, ...)

  structure(list(seed = seed, mat_synthetic = mat_synthetic), class = "synthetic_data")
}

.generate_synthetic_data_from_seed <- function(mat, init, base_seed = 10, ...){
  stopifnot(class(init) == "generator_init")

  set.seed(base_seed)
  iter <- 1

  while(TRUE){
    idx_row <- sample(1:nrow(mat), max(round(init$distortion_param * stats::runif(1) * nrow(mat)), 2))
    mat[idx_row,] <- init$mixing_list[[sample(1:length(init$mixing_list), 1)]](mat[idx_row,,drop = F], ...)

    if(stats::runif(1) >= init$stopping_param) break()
    iter <- iter + 1
    if(iter > init$max_iter) break()
  }

  mat
}

generate_mixing_default <- function(function_starter = "mixing", class_name = "mixing_list",
                                    package_name = "statCognition"){
  .grab_package_contents(function_starter, class_name, package_name)
}

.grab_package_contents <- function(function_starter, class_name, package_name = "statCognition"){
  if(!isNamespaceLoaded(package_name)) stop(paste(package_name, "is not",
                                                  "currently loaded"))

  all.obj <- ls(paste0("package:", package_name))
  fun <- grep(paste0("^", function_starter, "_*"), all.obj, value = T)

  lis <- lapply(fun, function(x){
    eval(parse(text = paste0(package_name, "::", x)))
  })
  names(lis) <- fun

  structure(lis, class = class_name)
}
