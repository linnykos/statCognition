#' Correlation graph via Pearson
#'
#' @param mat data
#' @param threshold threshold
#' @param edge_count number of edges
#' @param ... not used
#'
#' @return adjacency matrix
#' @export
CG_pearson <- function(mat, threshold = 0.7, edge_count = NA, ...){
  stopifnot(threshold >= 0, threshold <= 1, is.matrix(mat))

  cor_mat <- stats::cor(mat)
  .threshold_correlation_matrix(cor_mat, threshold, edge_count)
}

#' Correlation graph via Kendall's tau
#'
#' @param mat data
#' @param threshold threshold
#' @param edge_count number of edges
#' @param ... not used
#'
#' @return adjacency matrix
#' @export
CG_kendalls_tau <- function(mat, threshold = 0.7, edge_count = NA, ...){
  cor_mat <- stats::cor(mat, method = "kendall")
  .threshold_correlation_matrix(cor_mat, threshold, edge_count)
}

#' Correlation graph via Distance correlation
#'
#' @param mat data
#' @param threshold threshold
#' @param edge_count number of edges
#' @param ... not used
#'
#' @return adjacency matrix
#' @export
CG_distance_correlation <- function(mat, threshold = 0.7, edge_count = NA, ...){
  pairs <- utils::combn(ncol(mat), 2)

  cor_mat <- matrix(1, ncol(mat), ncol(mat))
  for(i in 1:ncol(pairs)){
    cor_mat[pairs[1,i], pairs[2,i]] <- energy::dcor(mat[,pairs[1,i]], mat[,pairs[2,i]])
    cor_mat[pairs[2,i], pairs[1,i]] <- cor_mat[pairs[1,i], pairs[2,i]]
  }

  .threshold_correlation_matrix(cor_mat, threshold, edge_count)
}

#' Correlation graph via Mutual information
#'
#' @param mat data
#' @param threshold threshold
#' @param edge_count number of edges
#' @param numBins discretization level
#' @param ... not used
#'
#' @return adjacency matrix
#' @export
CG_discrete_mutual_information <- function(mat, threshold = 0.7, edge_count = NA,
                                           numBins = 5, ...){
  pairs <- utils::combn(ncol(mat), 2)

  cor_mat <- matrix(1, ncol(mat), ncol(mat))
  for(i in 1:ncol(pairs)){
    bins <- entropy::discretize2d(mat[,pairs[1,i]], mat[,pairs[2,i]],
                                  numBins1 = numBins, numBins2 = numBins)
    cor_mat[pairs[1,i], pairs[2,i]] <- entropy::mi.empirical(bins)
    cor_mat[pairs[2,i], pairs[1,i]] <- cor_mat[pairs[1,i], pairs[2,i]]
  }

  .threshold_correlation_matrix(cor_mat, threshold*.compute_maximum_entropy(mat, numBins), edge_count)
}

######

.compute_maximum_entropy <- function(mat, numBins){
  max(apply(mat, 2, function(x){
    entropy::entropy(entropy::discretize(x, numBins = numBins))
  }))
}

.threshold_correlation_matrix <- function(cor_mat, threshold, edge_count){
  if(!is.na(edge_count)) threshold <- sort(abs(cor_mat)[upper.tri(cor_mat)], decreasing = T)[edge_count]

  cor_mat[abs(cor_mat)>=threshold] <- 1
  cor_mat[abs(cor_mat)<=threshold] <- 0

  cor_mat
}

