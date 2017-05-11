synthetic_initializer <- function(func_list = .grab_package_contents("generator")){
  structure(func_list, class = "synthetic_initializer")
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
