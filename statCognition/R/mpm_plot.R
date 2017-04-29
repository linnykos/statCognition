plot.mpm <- function(x, line_col = 2, plot_data = T, ...){
  if(plot_data & all(!is.na(x$data))){
    graphics::plot(x$breakpoints, x$data, pch = 1, col = 1, ...)
    graphics::points(x$breakpoints, x$values, pch = 16, col = line_col, ...)
  } else {
    graphics::plot(x$breakpoints, x$values, pch = 16, col = line_col, ...)
  }

  idx <- c(1, .find_breakpoints(x$values), length(x$values))
  for(i in 2:length(idx)){
    graphics::lines(x$breakpoints[idx[i-1]:idx[i]], rep(x$values[idx[i-1]], idx[i]-idx[i-1]+1),
                    col = line_col, ...)
    graphics::lines(rep(x$breakpoints[idx[i]], 2), x$values[idx[c(i-1, i)]],
                   col = line_col, ...)
  }

  invisible()
}

#defined as right breakpoint, i.e., c(1,1,2) has a breakpoint at 3
.find_breakpoints <- function(val, tol = 1e-4){
  idx <- which(abs(diff(val)) >= tol)
  if(length(idx) > 0) return(idx + 1) else return(1)
}
