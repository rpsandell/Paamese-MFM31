## Readme to accompany files for learnability simulations of Paamese-type stress patterns

## All files are R scripts, tested in R v. 4.4.2

## Place all files and folders in a single folder, set your working directory to that folder.

## The data used for learning simulations can be generated with the file GeneratingStressPatterns_Paamese_V2.R
	# However, it is not necessary to run this script in order to run the learnability simulations.
	# The files generated by this script are found in the folder PaameseTypePatterns

## All simulations are run in PaameseTypeLearnabilitySimulations.R
	## The outputs of the function simple_learning_loop_maxent_sga() are list objects.
	## Documentation of what information is found at which indices in the output list objects, assuming an object
	## named "test" created by the following function call:
		# test <- simple_learning_loop_maxent_sga()
		# test[[1]][[N]]: results for some generation N
			#  test[[1]][[N][[1]]: a list object in which each element is named after a UR, and the predicted probabilities of all candidates belong to that UR at the end of that generation.
			# test[[1]][[N]][[2]]: weights of all constraints at the end of generation N
			# test[[1]][[N]][[3]]: weights of constraints after each sampling of a token for learning, regardless of whether weights were updated or not.
			# test[[1]][[N]][[4]]: number of updates to constraint weights in that generation of learning
			# test[[1]][[N]][[5]]: Sum squared error of all stress patterns contained in the dataframe "stress_patterns for_SSE" (see step 2. in PaameseTypeLearnabilitySimulations) at intervals of 100 tokens
		# test[[2]]: constraint weights at the end of the final generation of training (same as test[[1]][[N]][[2]] when N is the final generation of training.
		# test[[3]]: a dataframe representing the output probabilities for all overt forms at the end of the final generation of training
		# test[[4]]:  Sum squared error of all stress patterns contained in the dataframe "stress_patterns for_SSE" (see step 2. in PaameseTypeLearnabilitySimulations) at the end of the final generation of training.
