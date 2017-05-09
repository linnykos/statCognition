difference_states_indicator <- function(dat_1, dat_2){
  if(all.equal(get_primary(dat_1), get_primary(dat_2))) return(1) else return(0)
}

all.equal.matrix <- function(target, current, tol = 1e-4, ...){
  if(any(dim(target) != dim(current))) return(FALSE)
  if(sum(abs(target - current)) < tol) return(TRUE) else return(FALSE)
}
