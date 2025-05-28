# Calculate the probability of candidates using maximum entropy given the weights of the constraints
# and the violation profiles of the candidates
get_cand_prob <- function(violations, weights){
  # Because vectors are "recycled" by column, violation vectors need to be transposed to be arranged by column, rather than row, then transposed back.
  applied_weights <- t(t(violations)*weights) # Calculate the "force" of each violation
  harmonies = apply(applied_weights, 1, sum) # Sum across each row to obtain the Harmony (H) of each candidate.
  e_harmonies = exp(-1*harmonies) # Take the exponential of the harmonies (e-harmony)
  Z = sum(e_harmonies) # Sum the e-harmonies to obtain the denominator (Z) for calculating candidate probabilities
  cand_probs <- e_harmonies/Z # Candidate probabilites are the e-harmony of each candidate divided by their sum.
  return(cand_probs)
}