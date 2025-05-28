# Update of constraint weights with delta rule (Perceptron)
# Any resulting negative weights are set to 0 when negative_weights is set to FALSE.
update_rule <- function(teacher_winner, learner_winner, rate, learner_weights, negative_weights = F){
  change_in_weights <- rate*(learner_winner - teacher_winner) # Cf. Jarosz 2016: 204
  learner_weights <- learner_weights + change_in_weights
  if(negative_weights == F){
    negatives <- which(learner_weights < 0) # Contrast GLA, which allows negative weights.
    learner_weights[negatives] <- 0
  }
  return(learner_weights)
}