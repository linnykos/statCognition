synthetic_generator <- function(dat, init){
  stopifnot(class(dat) == "data", class(init) == "synthetic_initializer")

  seed <- round(stats::runif(1)*1000)

  dat_synthetic <- .synthetic_generator_seed(dat, init, seed = seed)

  dat_synthetic$synthetic_seed <- seed

  dat_synthetic
}

get_seed <- function(dat){
  if("synthetic_seed" %in% names(dat)){
    dat$synthetic_seed
  } else {
    warning("Input dat does not have a seed, hence is not synthetic")
    invisible()
  }
}

.synthetic_generator_seed <- function(dat, init, seed = 10){
  stopifnot(class(dat) == "data", class(init) == "synthetic_initializer")

  set.seed(seed)
  lambda <- attr(init, "lambda"); if(is.null(lambda)) lambda <- 5
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
