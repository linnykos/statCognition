value_evaluate <- function(obj, vec, return.value = T){
  stopifnot(class(obj) == "value", .num_states(obj) == length(vec))

  #find which block
  idx <- sapply(1:length(vec), function(x){
    max(which(obj$surface$block_list[[x]] <= vec[x]))
  })

  key <- paste0(idx, collapse = "-")
  val <- obj$surface$hash[[key]]
  if(return.value) val$value else val$action
}
