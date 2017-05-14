#' Plotting function for contribution
#'
#' @param x contribution object
#' @param jitter boolean for jittering x-axis
#' @param samples boolean for plotting samples
#' @param contribution boolean splitting samples into total value and contribution
#' @param lwd plotting \code{lwd} parameter for samples
#' @param line_lwd plotting \code{lwd} parameter for fit
#' @param ... other plotting parameters
#'
#' @return void
#' @export
plot.contribution <- function(x, jitter = T, samples = F, contribution = F,
                              lwd = 1, line_lwd = 3, ...){
  if(samples) stopifnot(!any(is.na(x$samples)))

  if(!any(is.na(x$samples))){
    xvec <- x$samples[,1]; yvec1 <- x$samples[,2]; yvec2 <- x$samples[,3]
  } else {xvec <- NA; yvec1 <- NA; yvec2 <- NA}

  xlim <- c(min(c(x$breakpoints, xvec), na.rm = T),
            max(c(x$breakpoints, xvec), na.rm = T))
  ylim <- c(min(c(x$values, yvec1, yvec2, 0), na.rm = T),
            max(c(x$values, yvec1, yvec2), na.rm = T))

  graphics::plot(NA, xlim = xlim, ylim = ylim, ...)
  if(samples){
    n <- length(xvec)
    if(jitter) jit <- stats::rnorm(n, sd = diff(range(xlim))/500) else jit <- rep(0,n)

    if(contribution){
      #draw lines
      sapply(1:n, function(i){
        graphics::lines(x = rep(xvec[i] + jit[i], 2), y = c(ylim[1], yvec1[i]),
                        col = grDevices::rgb(0.7, 0.7, 0.7), lwd = lwd, ...)
        invisible()
      })

      sapply(1:n, function(i){
        graphics::lines(x = rep(xvec[i] + jit[i], 2), y = c(ylim[1], yvec2[i]),
                        col = "blue", lwd = lwd, ...)
        invisible()
      })
    }

    #plot points
    graphics::points(x = xvec + jit, y = yvec1, col = grDevices::rgb(0.7, 0.7, 0.7), ...)
  }

  #plot line
  m <- length(x$breakpoints)
  breakpoints <- c(-10*abs(xlim[1]), x$breakpoints, 10*abs(xlim[2]))
  val <- c(x$values[1], x$values, x$values[m])
  m <- m+2
  for(i in 1:(m-1)){
    graphics::lines(x = breakpoints[i:(i+1)], y = rep(val[i], 2), col = "red", lwd = line_lwd, ...)
    graphics::lines(x = rep(breakpoints[i+1], 2), y = rep(val[i:(i+1)]), col = "red", lwd = line_lwd, ...)
  }

  invisible()
}
