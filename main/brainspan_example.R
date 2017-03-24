load("../main/assets/brainspan_PFC35_top50gene_extract.RData")

mat <- mat[,1:10]
tmp_graph <- stats::cor(mat)
diag(tmp_graph) <- 0
quantile(abs(as.numeric(tmp_graph)))
(length(which(abs(tmp_graph) >= 0.4))/2)/(10*9/2)

#induce some randomness
set.seed(10)
mat[sample(1:nrow(mat), 15),1] <- NA
mat[sample(1:length(mat), 15)] <- NA

sum(apply(mat, 1, function(x){any(is.na(x))}))

#grab the phenotypes
pheno <- read.csv("../../DUMP/data/brainspan/brainseq.info.txt", sep = "\t")
#get the names
nam <- sapply(rownames(mat), function(x){
  strsplit(x, split = "\\.")[[1]][1]
})
idx <- which(pheno$Braincode %in% unique(nam))
pheno <- pheno[idx, c("Braincode", "Gender", "Days")]
rownames(pheno) <- pheno$Braincode
pheno <- pheno[,c("Gender", "Days")]

#construct the phenotype matrix
pheno <- as.data.frame(t(sapply(nam, function(x){
  pheno[which(rownames(pheno) == x),]
})))

##########################
#grab all the methods
MV_list <- .grab_package_contents("MV", "MV_list")
SS_list <- .grab_package_contents("SS", "SS_list")
RC_list <- .grab_package_contents("RC", "RC_list")
CG_list <- .grab_package_contents("CG", "CG_list")
methods_pairs <- expand.grid(1:4, 1:4, 1:4, 1:4)

igraph_list <- sapply(1:length(methods_pairs), function(x){
  vec <- as.numeric(methods_pairs[x,])
  res <- MV_list[[vec[1]]](mat)
  res <- SS_list[[vec[2]]](res)
  res <- RC_list[[vec[3]]](res, pheno)
  res <- CG_list[[vec[4]]](res, edge_count = 16)
  igraph::graph_from_adjacency_matrix(res)
})

