load("../main/assets/brainspan_PFC35_top50gene_extract.RData")
load("../main/assets/brainspan_PFC35_phenotype.RData")

combn_mat <- utils::combn(50, 2)
num_pages <- ceiling(ncol(combn_mat)/25)
idx <- 1
col_vec <- sapply(1:nrow(pheno), function(x){
  rgb((pheno[x,2]-84)/(119-84), 0, (119-pheno[x,2])/(119-84))
})

for(i in 1:num_pages){
  png(paste0("../res/enumerate_scatterplot/", i, ".png"), height = 5, width = 5,
      units = "in", res = 300)
  par(mar = rep(0.5, 4), mfrow = c(5,5))

  for(j in idx:min(idx+24, ncol(combn_mat))){
    plot(mat[,combn_mat[1,j]], mat[,combn_mat[2,j]], col = col_vec, pch = 16,
         xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  }
  graphics.off()

  idx <- idx+25
}

j = 236
plot(mat[,combn_mat[1,j]], mat[,combn_mat[2,j]], col = col_vec, pch = 16,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
