.plot_value_contribution_surface <- function(x = x, y = y, z = z, theta = 30, phi = 30,
                                             expand = 0.5, col = "lightblue",
                                             ticktype = "detailed", ...){
  graphics::persp(x, y, z, theta = theta, phi = phi, expand = expand, col = col,
                  ticktype = ticktype, xlab = names(x)[1], ylab = names(x)[2], ...)

  invisible()
}

.plot_value_surface <- function(x, ...){
  invisible()
}

#######################################

.compute_surface <- function(x, tol = 1e-4){
  #set up the axes
  xvec <- x[[1]]$breakpoints; yvec <- x[[2]]$breakpoints
  xval <- x[[1]]$values; yval <- x[[2]]$values

  #adjust
  xvec <- .pad_vector(xvec, tol = tol); xval <- c(xval[1], rep(xval, each = 2))
  yvec <- .pad_vector(yvec, tol = tol); yval <- c(yval[1], rep(yval, each = 2))

  #set up the height
  z <- outer(xval, yval, "+")

  list(xvec = xvec, yvec = yvec, z = z)
}


#does two things: 1) extend the min and max value, 2) duplicated all values with -tol
.pad_vector <- function(vec, tol = 1e-4){
  stopifnot(all(vec == sort(vec, decreasing = F)))
  if(length(vec) == 1) return(c(vec-1,vec,vec+1))

  m <- length(vec)
  width <- diff(range(vec))/20

  tmp <- as.numeric(sapply(2:m, function(x){vec[x] - c(tol,0)}))
  tmp <- c(vec[1], tmp)

  c(tmp[1]-width, tmp, tmp[length(tmp)]+width)
}
