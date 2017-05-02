load("../main/assets/brainspan_PFC35_top50gene_extract.RData")
load("../main/assets/brainspan_PFC35_phenotype.RData")

combn_mat <- utils::combn(50, 2)
pheno$Days <- 1:nrow(pheno)
pheno <- data.frame(Days = pheno$Days)

#####################
SS_list <- .grab_package_contents("SS", "SS_list")[c(1,3)]

SS_list[[2]] <- function(mat, pheno){

  tmp_mat <- cbind(mat[,c(1,2)]); colnames(tmp_mat) <- c("V1", "V2"); tmp_mat <- as.data.frame(tmp_mat)
  res <- stats::lm(V1~V2, data = tmp_mat)
  idx <- which(stats::cooks.distance(res) < 1)

  list(mat = mat[idx,], pheno = pheno[idx,])
}

RC_list <- .grab_package_contents("RC", "RC_list")[c(1,3)]

pearson <- function(mat){
  cor(mat[,1], mat[,2])
}
kendall <- function(mat){
  cor(mat[,1], mat[,2], method = "kendall")
}
CG_list <- list(pearson = pearson, kendall = kendall)

######################
methods_pairs <- expand.grid(1:2, 1:2, 1:2)
res_mat <- matrix(0, ncol = 8, nrow = ncol(combn_mat))

for(i in 1:ncol(combn_mat)){
  dat <- mat[,combn_mat[,i]]

  for(x in 1:nrow(methods_pairs)){
    vec <- as.numeric(methods_pairs[x,])
    res <- RC_list[[vec[1]]](dat, pheno)
    res <- SS_list[[vec[2]]](res, pheno)
    res_mat[i,x] <- CG_list[[vec[3]]](res$mat)
  }

  if(i %% floor(ncol(combn_mat)/10) == 0) cat('*')
}

#########################

vec <- sapply(1:nrow(res_mat), function(x){
  res <- ks.test(res_mat[x,], seq(min(res_mat[x,]), max(res_mat[x,]), length.out = 8), alternative = "two.sided")
  res$p.value
})

vec <- apply(res_mat, 1, function(x){diff(range(x))})
which.max(vec)

vec <- apply(res_mat, 1, function(x){abs(x[1] - x[3])})
which.max(vec)
