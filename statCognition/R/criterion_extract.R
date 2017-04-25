criterion_extract_data_default <- function(mat, pheno, ...){
  clist <- .grab_package_contents("criterion_data", "criterion_data_list")
  sapply(1:length(clist), function(x){clist[[x]](mat, pheno = pheno, ...)})
}
