# Calculate the probabilities of all candidates in all tableaux in the data.
# Calls get_cand_prob()
initial_learner_candidate_probs <- function(learner_violations = learner_violations, learner_weights = learner_weights){
  learner_candidate_probs <- list()
  for(i in 1:length(learner_violations)){
    current_candidate_probs <- get_cand_prob(violations = learner_violations[[i]], weights = learner_weights)
    learner_candidate_probs[[i]] <- current_candidate_probs
  }
  return(learner_candidate_probs)
}