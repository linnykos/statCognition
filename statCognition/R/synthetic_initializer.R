synthetic_initializer <- function(func_list = .grab_package_contents("generator")){
  structure(func_list, class = "synthetic_initializer")
}

#' Checks synthetic_initializer object for validity
#'
#' Runs the functions on the minimum and maximum values on the dataset
#'
#' @param obj The object to check
#' @param ... not used
#'
#' @return boolean
#' @export
is_valid.synthetic_initializer <- function(obj, dat, ...){
  len <- length(obj)
  tmp <- sapply(1:len, function(x){
    lis <- .synthetic_arg_grabber(obj[[x]])
    vec <- .generate_parameter_values(lis, func = min)
    tmp <- .apply_generator2dat(dat, obj[[x]], vec)
    if(class(dat) == "data_obj") stop(paste0(names(obj)[x], ", function number ", x,
                                             " does not work with the provided dat."))
  })

  tmp <- sapply(1:len, function(x){
    lis <- .synthetic_arg_grabber(obj[[x]])
    vec <- .generate_parameter_values(lis, func = max)
    tmp <- .apply_generator2dat(dat, obj[[x]], vec)
    if(class(dat) == "data_obj") stop(paste0(names(obj)[x], ", function number ", x,
                                             " does not work with the provided dat."))
  })

  TRUE
}

.apply_generator2dat <- function(dat, func, vec){
  len <- length(vec)
  eval(parse(text = paste0("func(dat, ",
                           paste0("param", 1:len, "=", vec, collapse = ","), ")")))
}

.generate_parameter_values <- function(lis, func = function(x){stats::runif(1, min = x[1], max = x[2])}){
  stopifnot(is.list(lis))
  stopifnot(all(sapply(lis, length) == 2))
  stopifnot(all(sapply(lis, is.numeric)))

  sapply(lis, func)
}

.grab_package_contents <- function(function_starter, package_name = "statCognition"){
  if(!isNamespaceLoaded(package_name)) stop(paste(package_name, "is not",
                                                  "currently loaded"))

  all.obj <- ls(paste0("package:", package_name))
  fun <- grep(paste0("^", function_starter, "_*"), all.obj, value = T)

  lis <- lapply(fun, function(x){
    eval(parse(text = paste0(package_name, "::", x)))
  })
  names(lis) <- fun

  lis
}

