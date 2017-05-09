difference_states_indicator <- function(dat_1, dat_2){
  if(all.equal(get_primary(dat_1), get_primary(dat_2))) return(1) else return(0)
}