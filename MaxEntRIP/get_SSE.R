# Simple function for calculating the sum squared error on two vectors of frequencies.
get_SSE <- function(vector_1, vector_2){
  return(sum((vector_1 - vector_2 )^2 ))
}