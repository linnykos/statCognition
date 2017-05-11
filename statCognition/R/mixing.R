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
