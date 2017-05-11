load("../main/assets/brainspan_PFC35_top50gene_extract.RData")
load("../main/assets/brainspan_PFC35_phenotype.RData")

mat <- mat[,c(3,6)]
pheno$Days <- 1:nrow(pheno)
pheno <- data.frame(Days = pheno$Days)
dat <- data_object(list(mat = mat, pheno = pheno))

par(mfrow = c(5,5), mar = rep(0.1, 4))
compute_color <- function(x){
  sapply(1:nrow(dat$pheno), function(x){
    rgb((dat$pheno[x,1])/107, 0, (107-dat$pheno[x,1])/107)
  })
}
#plot original data
plot(dat$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
     col = compute_color(dat))

#plot 20 synthetic datasets
init <- synthetic_initializer()
set.seed(10)
for(i in 1:20){
  res <- synthetic_generator(dat, init)
  plot(res$mat[,1:2], xlab = "", ylab = "", pch = 16, yaxt = "n", xaxt = "n", asp = T,
       col = compute_color(res))
  print(get_seed(res))
}
