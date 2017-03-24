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

