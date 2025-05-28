# Helper function for getting the probabilities of identical overt forms based on a grammar.
# Thus, crucially, if a grammar generates different foot structures with some probability, but which are identical in terms of their overt primary stress
# these probabilities will be added together.
get_position_probabilities <- function(learning_output, tableau_number = 1){
  candidates_no_ft_structure <- gsub("[\\(\\)]", "", names(learning_output[[1]][[tableau_number]]))
  learning_output_temp <- learning_output
  names(learning_output_temp[[1]][[tableau_number]]) <- candidates_no_ft_structure
  candidates_no_ft_structure <- unique(candidates_no_ft_structure)
  surface_output_probabilities <- c()
  for(candidate in candidates_no_ft_structure){
    summed_probs <- sum(learning_output_temp[[1]][[tableau_number]][which(names(learning_output_temp[[1]][[tableau_number]]) == candidate)])
    surface_output_probabilities <- append(surface_output_probabilities, summed_probs)
  }
  names(surface_output_probabilities) <- candidates_no_ft_structure
  return(surface_output_probabilities)
}