synthetic_generator <- function(dat, init, lambda = 5){
  stopifnot(class(dat) == "data", class(init) == "synthetic_initializer")

  seed <- round(stats::runif(1)*1000)

  dat_synthetic <- .synthetic_generator_seed(dat, init, lambda, seed = seed)

  dat_synthetic$synthetic_seed <- seed
  dat_synthetic
}

.synthetic_generator_seed <- function(dat, init, lambda = 5, seed = 10){
  stopifnot(class(dat) == "data", class(init) == "synthetic_initializer")

  set.seed(seed)
  iters <- stats::rpois(1, lambda); len <- length(init)

  for(i in 1:iters){
    set.seed(seed+i)
    idx <- sample(1:len, 1); func <- init[[idx]]
    lis <- .synthetic_arg_grabber(func)
    vec <- .generate_parameter_values(lis)
    dat <- .apply_generator2dat(dat, func, vec)
  }

  dat
}
