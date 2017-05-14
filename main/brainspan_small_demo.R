load("../main/assets/brainspan_PFC35_top50gene_extract.RData")
load("../main/assets/brainspan_PFC35_phenotype.RData")

dat <- mat[,c(3,6)]
pheno$Days <- 1:nrow(pheno)
pheno <- data.frame(Days = pheno$Days)

#####################
SS_list <- vector("list", 2)

SS_list[[1]] <- function(mat, pheno = NA, quantile = 0.25, neighbor_threshold = 1, ...){
  mat_scale <- scale(mat)
  dis <- stats::dist(mat_scale)
  rad <- stats::quantile(dis, probs = quantile)
  nn <- dbscan::frNN(mat_scale, eps = rad)

  bool <- sapply(nn$id, function(x){
    if(length(x) <= neighbor_threshold) FALSE else TRUE
  })

  mat <- mat[which(bool),,drop = F]
  if(!any(is.na(pheno))) pheno <- pheno[which(bool),,drop = F]

  list(mat = mat, pheno = pheno)
}
SS_list[[2]] <- function(mat, pheno){

  tmp_mat <- cbind(mat[,c(1,2)]); colnames(tmp_mat) <- c("V1", "V2"); tmp_mat <- as.data.frame(tmp_mat)
  res <- stats::lm(V1~V2, data = tmp_mat)
  vec <- stats::cooks.distance(res)
  cutoff <- 1.5*stats::IQR(vec) + stats::median(vec)

  idx <- which(vec < cutoff)

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
res_list <- vector("list", nrow(methods_pairs))

col_vec <- sapply(1:nrow(pheno), function(x){
  rgb((pheno[x,1])/107, 0, (107-pheno[x,1])/107)
})

png(paste0("../res/enumerate_example/base.png"), height = 1, width = 1.5,
    units = "in", res = 700)
par(mar = rep(0.1,4))
plot(dat[,1], dat[,2], pch = 16, col = col_vec,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
graphics.off()

#####################

for(x in 1:nrow(methods_pairs)){
  vec <- as.numeric(methods_pairs[x,])
  res <- RC_list[[vec[1]]](dat, pheno)

  png(paste0("../res/enumerate_example/row-", x, "-step-1.png"), height = 1, width = 1.5,
      units = "in", res = 700)
  par(mar = rep(0.1,4))
  plot(res[,1], res[,2], pch = 16, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  graphics.off()

  xlim <- range(res[,1]); ylim <- range(res[,2])
  res <- SS_list[[vec[2]]](res, pheno)

  png(paste0("../res/enumerate_example/row-", x, "-step-2.png"), height = 1, width = 1.5,
      units = "in", res = 700)
  par(mar = rep(0.1,4))
  plot(res$mat[,1], res$mat[,2], pch = 16, xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       xlim = xlim, ylim = ylim)
  graphics.off()

  res_list[[x]] <- CG_list[[vec[3]]](res$mat)
}
