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
    for(i in 1:a){
      #reorganize
      fun <- function(j){x$contribution_ll[[j]][[i]]}
      contribution_list <- lapply(1:d, fun)
      names(contribution_list) <- names(x$contribution_ll)

      #plot
      .plot_value_contribution_surface(contribution_list, main = names(x$contribution_ll[[1]])[i], ...)
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
