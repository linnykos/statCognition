cognition_state <- function(){
  .grab_package_contents("^state_data", "state_list")
}

.apply_cognition_state <- function(state_list, mat, pheno, ...){
  sapply(1:length(state_list), function(x){state_list[[x]](mat, pheno = pheno, ...)})
}
