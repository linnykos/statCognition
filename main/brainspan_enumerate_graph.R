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
pheno$Gender <- as.factor(as.character(pheno$Gender))
pheno$Days <- as.numeric(pheno$Days)

#construct the phenotype matrix
pheno <- as.data.frame(t(sapply(nam, function(x){
  pheno[which(rownames(pheno) == x),]
})))
pheno <- data.frame(Gender = as.factor(as.character(pheno$Gender)), Days = as.numeric(pheno$Days))

##########################
#grab all the methods
MV_list <- .grab_package_contents("MV", "MV_list")
SS_list <- .grab_package_contents("SS", "SS_list")
RC_list <- .grab_package_contents("RC", "RC_list")
CG_list <- .grab_package_contents("CG", "CG_list")
methods_pairs <- expand.grid(1:4, 1:4, 1:4, 1:4)

igraph_list <- vector("list", nrow(methods_pairs))
for(x in 1:nrow(methods_pairs)){
  vec <- as.numeric(methods_pairs[x,])
  res <- MV_list[[vec[1]]](mat, pheno)
  res <- SS_list[[vec[2]]](res$mat, res$pheno)
  res <- RC_list[[vec[3]]](res$mat, res$pheno)
  res <- CG_list[[vec[4]]](res, edge_count = 16)

  rownames(res) <- NULL; colnames(res) <- NULL
  g <- igraph::graph_from_adjacency_matrix(res)
  igraph_list[[x]] <- igraph::as.undirected(g)

  if(x %% floor(nrow(methods_pairs)/10) == 0) cat('*')
}

for(i in 1:nrow(methods_pairs)){
  pdf(paste0("../res/plot_",i,".pdf"), width = 5, height = 5)
  par(mar = rep(0,4))
  E(igraph_list[[i]])$color <- "black"
  plot(igraph_list[[i]], layout = layout.circle, edge.width = 2)
  graphics.off()
}


