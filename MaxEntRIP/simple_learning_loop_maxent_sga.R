# Stochastic Gradient Ascent for learning constraint weights that produce structured (footed) from overt representations
# with Robust Interpretive Parsing. Winning candidates are determined via maximum entropy.
# Single-agent iterated learning possible when number_generations > 1.
# Set Use_conv_cond = FALSE to learn from a fixed number of tokens.
simple_learning_loop_maxent_sga <- function(tableaux = my_tableaux, overts = my_overts, number_iterations = 5000, learning_rate = 0.1, number_generations = 1, initial_weights = my_learner_weights, stress_sample_probs = my_stress_sample_probs, save_intermediate_SSEs = TRUE, Use_conv_cond = TRUE, SSE_convergence = 1){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  current_generation <- 1
  generation_results <- list()
  learner_weights_frame <- rbind(initial_weights)
  if(current_generation == 1){
    my_learner_violations <- tableaux[[2]]
    learner_cand_probs <- initial_learner_candidate_probs(my_learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
    learner_weights <- initial_weights # set the initial weights to the starting weights for the first generation of learning
    iteration_weights <- list() # keep track of the weight evolution within the learning generation
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    intermediate_SSE_results <- as.data.frame(rbind(rep(0, ncol(stress_patterns_for_SSE)-2))) # keep track of SSEs at every 100 tokens
    for(i in 1:number_iterations){
      if((i %% 10000) == 0){
        print(i) # Print an output every 10000 sampled tokens, just to signal that nothing has gone wrong.
      }
      

      sampled_item <- sample(overts, size = 1, prob = (stress_sample_probs/sum(stress_sample_probs)), replace=TRUE) # create an object my_stress_sample_probs in the global environment, or specify a vector of frequencies as stress_sample_probs in the function call
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      
      possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface.
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights) # update the constraint weights
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(my_learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      if(save_intermediate_SSEs == TRUE){
        if((i %% 100) == 0){ # Every 100 tokens, 
          final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates)
          names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names from the tableaux object
          names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
          for(i in 1:length(final_learner_outcome[[1]])){
            names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
          }
          learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
          colnames(learner_weights_frame) <- tableaux[[5]] # This is probably superfluous
          generation_results[[current_generation]] <- final_learner_outcome
          # Getting SSEs for stress patterns in the current generation
          current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
          for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
            # Obtain the output probabilities for all possible overt forms
            current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
            current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
          }
          # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
          if(length(current_generation_output_probabilities) == 502){
            stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
            #print("short correction!")
          }
          else{
            stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
          }
          current_SSE_results <- c()
          for(i in 3:ncol(stress_patterns_for_SSE)){
            temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
            current_SSE_results <- append(current_SSE_results, temp_SSE_results)
          }

          intermediate_SSE_results <- rbind(intermediate_SSE_results, current_SSE_results)
          colnames(intermediate_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
          # Check for satisfaction of the convergence condition, if used.
          if(Use_conv_cond & (current_SSE_results[(pattern_column_number - 2)] < SSE_convergence)){
            break # Exit the learning loop whenever the SSE of the current pattern first falls below 1
          }
        }
      }
      # End of learning loop
    }
    # Store all data from the current generation
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates, intermediate_SSE_results)
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- as.data.frame(rbind(learner_weights))
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      # Obtain the output probabilities for all distinct overt forms
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c()
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1
  }
  # Loop for iterated single-agent learning
  while(current_generation > 1 & current_generation <= number_generations){
    learner_cand_probs <- initial_learner_candidate_probs(my_learner_violations, initial_weights)
    learner_weights <- initial_weights
    iteration_weights <- list()
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # First sample from the original (empirical) distribution to obtain a prosodic shape
      sampled_item <- sample(overts, size = 1, prob = (stress_sample_probs/sum(stress_sample_probs)), replace=TRUE) 

      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress

      # Find the relevant tableau
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      # Now get a teacher output based on the probability distribution of the preceding generation
      # Remember to convert it into an overt form as well by removing junk
      teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])

      teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
      teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
      #teacher_token <- gsub("2", "", teacher_token) # remove secondary stress
      
      possible_winner_numbers <- grep(teacher_token, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate

      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface.
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights) # update the constraint weights

        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(my_learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      if(save_intermediate_SSEs == TRUE){
        if((i %% 100) == 0){ # Every 100 tokens, calculate the sum squared error for the stress patterns being evaluated
          final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
          names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
          names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
          for(i in 1:length(final_learner_outcome[[1]])){
            names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
          }
          learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
          colnames(learner_weights_frame) <- tableaux[[5]]

          generation_results[[current_generation]] <- final_learner_outcome
          # Getting SSEs for stress patterns in the current generation
          current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
          for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
            # Get the probabilities of all distinct overt forms
            current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
            current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
          }
          # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
          if(length(current_generation_output_probabilities) == 502){
            stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
          }
          else{
            stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
          }
          current_SSE_results <- c()
          for(i in 3:ncol(stress_patterns_for_SSE)){
            temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
            current_SSE_results <- append(current_SSE_results, temp_SSE_results)
          }
          intermediate_SSE_results <- rbind(intermediate_SSE_results, current_SSE_results)
          colnames(intermediate_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
          if(Use_conv_cond & (current_SSE_results[(pattern_column_number - 2)] < SSE_convergence)){
            break # Exit the learning loop whenever the SSE of the current pattern first falls below the convergence condition
          }
        }
      } 
      # End of learning loop
    }
    # Save the data
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]] # you had forgotten this!
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      # Get the probabilities of the distinct overt forms
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c()
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
    ### Here, a dataframe with final output frequencies is created, which will allow for straightforward comparison to the distributions in the initial state.
    final_output_frequencies <- as.data.frame(cbind(0,0,0,0)) # The dataframe needs four columns: the underlying abstract prosodic shape, corresponding overt forms, the output probability of the overt form, and the original empirical token frequencies
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[number_generations]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities)) # convert the overt forms to just a single shape, rather than a mapping
      for(output in 1:length(current_output_probabilities)){ # iterate again over the number of possible inputs
        #frequency_original_overt <- all_prosodic_shapes_frame$Freq[which(all_prosodic_shapes_frame$all_prosodic_shapes == output_overt_forms[output])] # find the original frequency of the current over forms
        current_overt_row <- which(stress_patterns_for_SSE$V2 == output_overt_forms[output])
        frequency_original_overt <- stress_patterns_for_SSE[, pattern_column_number][current_overt_row] # Specify the pattern column number in the global environment where the overt forms are selected.
        to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt) # put all four pieces in a vector
        final_output_frequencies <- rbind(final_output_frequencies, to_be_bound) # bind the vector to the dataframe
      }
    }    
    final_output_frequencies <- final_output_frequencies[-c(1), ] # remove the first row of the dataframe that contains only 0's
    colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq") # add column names
    final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability, but the overt form did not occur in the RV
    frequency_input_shapes <- c() # Now, a vector with the frequencies of the input UR shapes in the RV will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], as.numeric(final_output_frequencies$Original_Freq[input_shape]))
      frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
    }
    table_input_shape_frequencies <- table(frequency_input_shapes) # a table is created to sum these frequencies
    predicted_new_frequencies <- c() # Now, a vector with the predicted new frequencies will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
      input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
      predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
      predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
    }
    final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies) # These final new frequencies are bound to the dataframe
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies, total_SSE_results) # total_KLD_results. The final list contains four items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies; a dataframe with the SSEs for some possible stress patterns; and a dataframe with KLD for possible stress patterns
    # Find the intermediate SSE results under output[[1]][[5]]
    return(final_generation_results)
  }
}

