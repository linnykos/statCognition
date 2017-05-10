difference_states_indicator <- function(vec1, vec2){
  if(length(vec1) != length(vec2)) return(1)
  if(all(vec1 == vec2)) return(0) else return(1)
}
