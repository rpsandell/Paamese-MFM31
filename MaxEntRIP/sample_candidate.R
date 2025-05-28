# Function for sampling "winning" candidates from probability distributions provided in the data.
sample_candidate <- function(tableaux, token_freqs_of_types=F){
  # Can be improved by adding token frequencies of the types and then using that as a probability for the tableaux sampling
  if(token_freqs_of_types == T){
    current_tableaux <- sample(length(tableaux[[1]]), 1, prob = tableaux_probs)
  }
  else{
    current_tableaux <- sample(length(tableaux[[1]]), 1)
  }
  current_candidate <- sample(unlist(tableaux[[3]][current_tableaux]), size=1, prob = unlist(tableaux[[1]][current_tableaux]))
  return(current_candidate)
}