criterionData_influential_points <- function(mat, num_pairs = 50, ...){
  d <- ncol(mat)
  if(d*(d-1)/2 < num_pairs){
    pairs <- utils::combn(d, 2)
  } else {
    pairs <- .generate_possible_pairs(ncol(mat), num_pairs = num_pairs)
  }

  outliers <- apply(pairs, 2, function(x){
    tmp_mat <- cbind(mat[,x]); colnames(tmp_mat) <- c("V1", "V2"); tmp_mat <- as.data.frame(tmp_mat)
    res <- stats::lm(V1~V2, data = tmp_mat)
    idx <- which(stats::cooks.distance(res) >= 1)
  })

  length(unique(as.numeric(unlist(outliers))))
}

criterionData_nearest_neighbor <- function(mat, ...){
  mat <- scale(mat)
  dis <- as.matrix(stats::dist(mat))
  diag(dis) <- Inf

  vec <- sapply(1:ncol(dis), function(x){
    min(dis[x,])
  })

iqr <- stats::IQR(vec); mid <- stats::median(vec)
  sum(sapply(vec, function(x){
    ifelse(x >= mid + 1.5*iqr | x <= mid - 1.5*iqr, TRUE, FALSE)
  }))
}

######################

.generate_possible_pairs <- function(d, num_pairs){
  idx <- sample(1:(d*(d-1)/2), num_pairs, replace = F)

  .convert_idx_to_pairs(d, idx)
}

.convert_idx_to_pairs <- function(d, idx_vec){
  sum_idx_vec <- cumsum((d-1):1)

  sapply(idx_vec, function(x){
    idx <- min(which(sum_idx_vec >= x))
    idx2 <- d - (sum_idx_vec[idx] - x)
    c(idx, idx2)
  })
}
