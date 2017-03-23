SS_none <- function(mat, ...){
  mat
}

SS_three_sd <- function(mat, ...){
  idx <- apply(mat, 2, function(x){
    c(which(x >= mean(x) + 3*stats::sd(x)), which(x <= mean(x) - 3*stats::sd(x)))
  })
  idx <- unique(as.vector(unlist(idx)))

  if(length(idx) == 0) mat else mat[-idx,,drop = F]
}

SS_quantile <- function(mat, ...){
  idx <- apply(mat, 2, function(x){
    c(which.min(x), which.max(x))
  })
  idx <- unique(as.vector(unlist(idx)))

  mat[-idx,,drop = F]
}

SS_neighborhood <- function(mat, quantile = 0.9, neighbor_threshold = 1, ...){
  mat_scale <- scale(mat)
  dis <- stats::dist(mat_scale)
  radius <- stats::quantile(dis, probs = quantile)
  nn <- dbscan::frNN(mat_scale, eps = radius)

  bool <- sapply(nn, function(x){
    if(length(x) <= neighbor_threshold) FALSE else TRUE
  })

  mat[which(bool),,drop = F]
}
