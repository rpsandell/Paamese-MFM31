

### Learnability Simulations for the Class of Paamese-like Languages ###
## NOTA BENE: Run the code as is with given object names. Some function calls may depend on an object with a given name 
## being present in the global environment.

## 0. Set your working directory to a folder containing the subfolders "PaameseTypePatterns" and "MaxEntRIP"

## 1. Load the functions needed for RIP with MaxEnt SGA
library(ggplot2)
library(tidyr)

source("MaxEntRIP/prepare_data.R")
source("MaxEntRIP/sample_candidate.R")
source("MaxEntRIP/get_cand_prob.R")
source("MaxEntRIP/initial_learner_cand_probs.R")
source("MaxEntRIP/update_rule.R")
source("MaxEntRIP/get_SSE.R") 
source("MaxEntRIP/get_position_probabilities.R")
source("MaxEntRIP/simple_learning_loop_maxent_sga.R")

## 2. Read in tableaux for with prepare_data()
# prepare_data()
my_tableaux <- prepare_data("PaameseTypePatterns/PaameseTypeSimplified_RIP.txt")
my_tableaux <- prepare_data("PaameseTypePatterns/PaameseType_RIP.txt")



# N. B. constraint names are in [[5]]
## 2. Read in a table of defined overt winners for various stress patterns. 
stress_patterns_for_SSE <- read.table("PaameseTypePatterns/Paamese_Patterns_for_SSEs_V2.txt", header=T, sep="\t")
# Use this dataframe to create a list object containing the possible overt forms under a given stress pattern.
#Create a list with just the vector of possible overt forms allowed under a given stress pattern.
stress_pattern_overts_list <- list()
for(i in 3:ncol(stress_patterns_for_SSE)){
  temp_stress_vector <- stress_patterns_for_SSE[, i]
  temp_licit_positives <- which(temp_stress_vector == 1)
  temp_licit_overts <- stress_patterns_for_SSE[temp_licit_positives, 2]
  stress_pattern_overts_list[[i]] <- temp_licit_overts
}
names(stress_pattern_overts_list) <- colnames(stress_patterns_for_SSE)

## 2. Set initial weights and store the violation vectors in a separate object.
# For Paamese type languages, set all to 5 to start, except *S1 ("no stress on an unstressable syllable") at 0
my_learner_weights <- rep(5,ncol(my_tableaux[[2]][[1]]))
# *S1 is at index 12
my_learner_weights[12] <- 0

## 3. Calculate initial candidate probabilities with initial_learner_candidate_probs()
## This is also done internally in simple_learning_loop_maxent_sga(), but best to do it in the global environment as well
my_learner_violations <- my_tableaux[[2]]
my_learner_cand_probs <- initial_learner_candidate_probs(my_learner_violations, my_learner_weights)

##4. Set current overt forms as my_overts, and create an object pattern_column_number with the index in stress_pattern_overts_list
my_overts <- stress_pattern_overts_list$QI.Final
pattern_column_number <- 3

##5. Set frequencies of overt forms. This is crucial to set before running simple_learning_loop_maxent_sga().
my_stress_sample_probs <- rep(1, length(my_overts)) # Use this option when all overt forms are assumed to be equally likely.

## 6. Run MaxEnt SGA with simple_learning_loop_maxent_sga((), 1000 tokens. Can skip this step.
test_1 <- simple_learning_loop_maxent_sga(number_iterations = 1000)
## Check output of test_1
test_1[[1]][[1]][[5]]

### SIMULATIONS ###
## For evaluating the relative ease of theoretical learnability of patterns, it is only necessary to 
## change the settings under 4. above before calling simple_learning_loop_maxent_sga() with a given number of iterations.
## All warnings below can be safely ignored.
# Allows learning from up to 1,000,000 tokens but the convergence criterion will be reached much sooner in all cases.

## Fixed final stress

set.seed(478901)
my_overts <- stress_pattern_overts_list$QI.Final
pattern_column_number <- 3

collector_final <- list(c(), c(), c(), c())
for(i in 1:100){
  test.Pm.final <- simple_learning_loop_maxent_sga(number_iterations = 1000000)  
  collector_final[[1]][i] <- test.Pm.final[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_final[[2]][i] <- test.Pm.final[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_final[[3]][i] <- test.Pm.final[[4]][1] # Ending SSE
  collector_final[[4]][i] <- (nrow(test.Pm.final[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}

mean(collector_final[[2]]) #38.03 grammar updates
mean(as.numeric(collector_final[[3]])) #0.5343976 SSE when learning terminated.
mean(collector_final[[4]]) #214 total number of tokens sampled


set.seed(478901)
my_overts <- stress_pattern_overts_list$QI.Penult
pattern_column_number <- 4
#test.Pm.penult <- simple_learning_loop_maxent_sga(number_iterations = 100000)
#test.Pm.penult_few <- simple_learning_loop_maxent_sga(number_iterations = 100)
#View(test.Pm.penult[[1]][[1]][[5]])

collector_penult <- list(c(), c(), c(), c())
for(i in 1:100){
  test.Pm.penult <- simple_learning_loop_maxent_sga(number_iterations = 1000000) # Allow up to 1000000 tokens of training data
  collector_penult[[1]][i] <- test.Pm.penult[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_penult[[2]][i] <- test.Pm.penult[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_penult[[3]][i] <- test.Pm.penult[[4]][pattern_column_number-2] # Ending SSE
  collector_penult[[4]][i] <- (nrow(test.Pm.penult[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}
# Penalizing final stress explicitly with NonFin-Syll makes learning penultimate stress easier than final stress, if weights are non-zero in the initial state.
mean(collector_penult[[2]]) # 9.59 grammar updates
mean(as.numeric(collector_penult[[3]])) # 0.4046743 SSE when learning terminated.
mean(collector_penult[[4]]) # 126 total number of tokens sampled


set.seed(478901)
my_overts <- stress_pattern_overts_list$QI.Antepenult
pattern_column_number <- 6

collector_antepenult <- list(c(), c(), c(), c())
for(i in 1:100){
  test.Pm.antepenult <- simple_learning_loop_maxent_sga(number_iterations = 1000000) # Allow up to 1000000 tokens of training data
  collector_antepenult[[1]][i] <- test.Pm.antepenult[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_antepenult[[2]][i] <- test.Pm.antepenult[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_antepenult[[3]][i] <- test.Pm.antepenult[[4]][pattern_column_number-2] # Ending SSE
  collector_antepenult[[4]][i] <- (nrow(test.Pm.antepenult[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}
# Penalizing final stress explicitly makes learning penultimate stress easier than final stress, if weights are non-zero in the initial state.
mean(collector_antepenult[[2]]) # 131.55 grammar updates
mean(as.numeric(collector_antepenult[[3]])) # 0.8599846 SSE when learning terminated.
mean(collector_antepenult[[4]]) # 932 total number of tokens sampled

## Window systems reported in poster / handout
set.seed(478901)
my_overts <- stress_pattern_overts_list$Pm..2.Window
pattern_column_number <- 5 # 5 = Pm..2.Window

collector_2.Window <- list(c(), c(), c(), c())
for(i in 1:100){
  test.Pm.2.Window <- simple_learning_loop_maxent_sga(number_iterations = 1000000) # Allow up to 1000000 tokens of training data
  collector_2.Window[[1]][i] <- test.Pm.2.Window[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_2.Window[[2]][i] <- test.Pm.2.Window[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_2.Window[[3]][i] <- test.Pm.2.Window[[4]][pattern_column_number-2] # Ending SSE
  collector_2.Window[[4]][i] <- (nrow(test.Pm.2.Window[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}
# Penalizing final stress explicitly makes learning penultimate stress easier than final stress, if weights are non-zero in the initial state.
mean(collector_2.Window[[2]]) # 177.05 grammar updates
mean(as.numeric(collector_2.Window[[3]])) # 0.8828286 SSE when learning terminated.
mean(collector_2.Window[[4]]) # 2346 total number of tokens sampled


set.seed(478901)
my_overts <- stress_pattern_overts_list$Pm..3.Window
pattern_column_number <- 7 # 8 = Pm..3.Window

collector_3.Window <- list(c(), c(), c(), c())
for(i in 1:100){
  test_Pm.3.Window <- simple_learning_loop_maxent_sga(number_iterations = 1000000) # Allow up to 1000000 tokens of training data
  collector_3.Window[[1]][i] <- test_Pm.3.Window[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_3.Window[[2]][i] <- test_Pm.3.Window[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_3.Window[[3]][i] <- test_Pm.3.Window[[4]][pattern_column_number-2] # Ending SSE
  collector_3.Window[[4]][i] <- (nrow(test_Pm.3.Window[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}
# Penalizing final stress explicitly makes learning penultimate stress easier than final stress, if weights are non-zero in the initial state.
mean(collector_3.Window[[2]]) # 1165.19 grammar updates
mean(as.numeric(collector_3.Window[[3]])) # 0.9596097 SSE when learning terminated
mean(collector_3.Window[[4]]) # 18058 total number of tokens sampled

set.seed(478901)
my_overts <- stress_pattern_overts_list$Pm..4.Window
pattern_column_number <- 8 # 8 = Pm..4.Window

collector_4.Window <- list(c(), c(), c(), c())
for(i in 1:100){
  test_Pm.4.Window <- simple_learning_loop_maxent_sga(number_iterations = 1000000) # Allow up to 1000000 tokens of training data
  collector_4.Window[[1]][i] <- test_Pm.4.Window[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_4.Window[[2]][i] <- test_Pm.4.Window[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_4.Window[[3]][i] <- test_Pm.4.Window[[4]][pattern_column_number-2] # Ending SSE
  collector_4.Window[[4]][i] <- (nrow(test_Pm.4.Window[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}
mean(collector_4.Window[[2]]) #1663.12 grammar updates
mean(as.numeric(collector_4.Window[[3]])) #0.970864 SSE when learning terminated
mean(collector_4.Window[[4]]) #  38942 total number of tokens sampled

set.seed(478901)
my_overts <- stress_pattern_overts_list$Pm..5.Window
pattern_column_number <- 9 # 9 = Pm..5.Window

collector_5.Window <- list(c(), c(), c(), c())
for(i in 1:100){ # Only 52 runs of this pattern; run time is otherwise too slow 
  test_Pm.5.Window <- simple_learning_loop_maxent_sga(number_iterations = 1000000) # Allow up to 1000000 tokens of training data
  collector_5.Window[[1]][i] <- test_Pm.5.Window[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_5.Window[[2]][i] <- test_Pm.5.Window[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_5.Window[[3]][i] <- test_Pm.5.Window[[4]][pattern_column_number-2] # Ending SSE
  collector_5.Window[[4]][i] <- (nrow(test_Pm.5.Window[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}
mean(collector_5.Window[[2]]) # 3064.22 grammar updates
mean(as.numeric(collector_5.Window[[3]])) # 0.9725185 SSE when learning terminated
mean(collector_5.Window[[4]]) # 96512 total number of tokens sampled

set.seed(478901)
my_overts <- stress_pattern_overts_list$Pm..6.Window
pattern_column_number <- 10 # 10 = Pm..6.Window
test_Pm.6.Window <- simple_learning_loop_maxent_sga(number_iterations = 500000)
View(test_Pm.6.Window [[1]][[1]][[5]])

collector_6.Window <- list(c(), c(), c(), c())
for(i in 1:100){
  test_Pm.6.Window <- simple_learning_loop_maxent_sga(number_iterations = 1000000) # Allow up to 1000000 tokens of training data
  collector_6.Window[[1]][i] <- test_Pm.6.Window[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_6.Window[[2]][i] <- test_Pm.6.Window[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_6.Window[[3]][i] <- test_Pm.6.Window[[4]][pattern_column_number-2] # Ending SSE
  collector_6.Window[[4]][i] <- (nrow(test_Pm.6.Window[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}

mean(collector_6.Window[[2]]) # 3862.4 grammar updates
mean(as.numeric(collector_6.Window[[3]])) # 0.9751201 SSE when learning terminated
mean(collector_6.Window[[4]]) # 152880 total number of tokens sampled

set.seed(478901)
my_overts <- stress_pattern_overts_list$Unbounded.Right.Ante
pattern_column_number <- 11

collector_unbounded_right <- list(c(), c(), c(), c())
for(i in 1:100){
  test.unbounded.right <- simple_learning_loop_maxent_sga(number_iterations = 1000000)  
  collector_unbounded_right[[1]][i] <- test.unbounded.right[[1]][[1]][[5]] ## SSE progression by 100 tokens
  collector_unbounded_right[[2]][i] <- test.unbounded.right[[1]][[1]][[4]] # Total number of updates carried out at that point.
  collector_unbounded_right[[3]][i] <- test.unbounded.right[[4]][1] # Ending SSE
  collector_unbounded_right[[4]][i] <- (nrow(test.unbounded.right[[1]][[1]][[5]])-1)*100 # Number of training tokens
  print(i)
}

### Correlation of window size and number of grammar updates / number of tokens sampled. ONLY FOR WINDOW SYSTEMS.
window_sizes <- c(2,3,4,5,6)
grammar_updates <- c(177.05, 1165.19, 1663.12, 3064.22, 3862.4)
training_tokens <- c(2346, 18058, 38942, 96512, 152880)

# Correlation coefficients
cor.test(window_sizes, grammar_updates, method="pearson", alternative="greater")

cor.test(window_sizes, training_tokens, method="pearson", alternative="greater")

# Generalized linear models
updates_model <- glm(grammar_updates ~ window_sizes)
summary(updates_model)

tokens_model <- glm(training_tokens ~ window_sizes)
summary(tokens_model)

# In both models, the utility of the window size independent variable is greater than that of the intercept.

## Simulations for visualization: One simulation trained on 200000 tokens for each window pattern.
set.seed(478901)
# Window size 2
my_overts <- stress_pattern_overts_list$Pm..2.Window
pattern_column_number <- 5 # 5 = Pm..2.Window
window_2_sample <- simple_learning_loop_maxent_sga(number_iterations = 200000, Use_conv_cond = FALSE)
window_2_results <- window_2_sample[[1]][[1]][[5]]$Pm..2.Window

# Window size 3
my_overts <- stress_pattern_overts_list$Pm..3.Window
pattern_column_number <- 7 # 7 = Pm..3.Window
window_3_sample <- simple_learning_loop_maxent_sga(number_iterations = 200000, Use_conv_cond = FALSE)
window_3_results <- window_3_sample[[1]][[1]][[5]]$Pm..3.Window

# Window size 4
my_overts <- stress_pattern_overts_list$Pm..4.Window
pattern_column_number <- 8 # 8 = Pm..4.Window
window_4_sample <- simple_learning_loop_maxent_sga(number_iterations = 200000, Use_conv_cond = FALSE)
window_4_results <- window_4_sample[[1]][[1]][[5]]$Pm..4.Window

# Window size 5
my_overts <- stress_pattern_overts_list$Pm..5.Window
pattern_column_number <- 9 # 9 = Pm..5.Window
window_5_sample <- simple_learning_loop_maxent_sga(number_iterations = 200000, Use_conv_cond = FALSE)
window_5_results <- window_5_sample[[1]][[1]][[5]]$Pm..5.Window

# Window size 6
my_overts <- stress_pattern_overts_list$Pm..6.Window
pattern_column_number <- 10 # 10 = Pm..6.Window
window_6_sample <- simple_learning_loop_maxent_sga(number_iterations = 200000, Use_conv_cond = FALSE)
window_6_results <- window_6_sample[[1]][[1]][[5]]$Pm..6.Window

## PLOTS of SSE Trajectory
Tokens <- seq(100, 200000, by=100)
SSEs_All_Frame <- cbind(window_2_results[2:2001], window_3_results[2:2001],
                        window_4_results[2:2001], window_5_results[2:2001], window_6_results[2:2001])
colnames(SSEs_All_Frame) <- c("Two σ", "Three σ", "Four σ", "Five σ",
                              "Six σ")

## convert to long format with tidyr::pivot_longer
#SSEs_All_Frame_long_tidyr <- pivot_longer(SSEs_All_Frame, cols = c("Two_Window", "Three_Window", "Four_Window", "Five_Window",
#                                                              "Six_Window"))


SSEs_All_Frame_long <- melt(SSEs_All_Frame)
SSEs_All_Frame_final <- cbind(Tokens, SSEs_All_Frame_long)
SSEs_All_Frame_final <- SSEs_All_Frame_final[, c(1, 3, 4)]
colnames(SSEs_All_Frame_final) <- c("Sampled_Tokens", "Window_Size", "SSE")

# Color
Paamese_types_SSEs_smooth_plot_2 <- ggplot(SSEs_All_Frame_final, aes(x=Sampled_Tokens, y=log(SSE), color=Window_Size)) + geom_smooth() +
  geom_hline(yintercept = 0) + geom_point() + guides(color = guide_legend(title="Window Size")) + xlab("Number of Sampled Tokens") +
  theme(legend.text = element_text(size=9), legend.title = element_text(size=9),
        axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

Paamese_types_SSEs_smooth_plot_2

# Black and white with shapes
Paamese_types_SSEs_smooth_plot_3_shapes <- ggplot(SSEs_All_Frame_final, aes(x=Sampled_Tokens, y=log(SSE), shape=Window_Size)) + geom_smooth(aes(linetype = Window_Size), color = "black") +
  geom_hline(yintercept = 0) + geom_point(alpha = 0.25, size = 3) + theme_bw() +
  guides(color = guide_legend(title="Window Size")) + xlab("Number of Sampled Tokens") +
  theme(legend.text = element_text(size=9), legend.title = element_text(size=9),
        axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

Paamese_types_SSEs_smooth_plot_3_shapes






