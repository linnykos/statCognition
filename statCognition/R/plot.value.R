#' Plotting function for value
#'
#' @param x value object
#' @param type string for plot type
#' @param ... additional plotting parameters
#'
#' @return void
#' @export
plot.value <- function(x, type = "contribution", ...){
  stopifnot(type %in% c("contribution", "contribution_surface", "surface"))

  if(type == "contribution"){
    stopifnot(!any(is.na(x$contribution_ll)))
    .plot_value_contribution(x$contribution_ll, ...)

  } else if(type == "contribution_surface") {
    stopifnot(!any(is.na(x$contribution_ll)))

    d <- length(x$contribution_ll)
    a <- length(x$contribution_ll[[1]])
    surface_list <- vector("list", a)
    for(i in 1:a){
      #reorganize
      fun <- function(j){x$contribution_ll[[j]][[i]]}
      contribution_list <- lapply(1:d, fun)
      names(contribution_list) <- names(x$contribution_ll)

      #surface
      surface_list[[i]] <- .compute_surface(contribution_list)
    }

    #compute limits
    lim <- .compute_surface_limits(surface_list)

    for(i in 1:a){
      .plot_value_contribution_surface(x = surface_list[[i]]$xvec, y = surface_list[[i]]$yvec,
                                       z = surface_list[[i]]$z,
                                       main = names(x$contribution_ll[[1]])[i],
                                       xlim = lim$xlim, ylim = lim$ylim, zlim = lim$zlim, ...)
    }

  } else if(type == "surface"){

  }

  invisible()
}

.plot_value_contribution <- function(contribution_ll, contribution = F, ...){
  stopifnot(length(unique(sapply(contribution_ll, length))) == 1)

  d <- length(contribution_ll); a <- unique(sapply(contribution_ll, length))
  d_nam <- names(contribution_ll); a_nam <- names(contribution_ll[[1]])

  graphics::par(mfrow = c(d,a))
  for(i in 1:d){
    for(j in 1:a){
      plot.contribution(contribution_ll[[i]][[j]], main = paste0(d_nam[i],":",a_nam[j]),
                        contribution = contribution, ...)
    }
  }

  invisible()
}

.compute_surface_limits <- function(surface_list){
  len <- length(surface_list)
  xvec <- as.numeric(unlist(lapply(1:len, function(x){surface_list[[x]]$xvec})))
  xlim <- c(min(xvec), max(xvec))

  yvec <- as.numeric(unlist(lapply(1:len, function(x){surface_list[[x]]$yvec})))
  ylim <- c(min(yvec), max(yvec))

  zvec <- as.numeric(unlist(lapply(1:len, function(x){as.numeric(surface_list[[x]]$z)})))
  zlim <- c(min(zvec), max(zvec))

  list(xlim = xlim, ylim = ylim, zlim = zlim)
}
