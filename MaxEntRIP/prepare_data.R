# Read in tableaux and get candidate frequencies plus candidate violation profiles
prepare_data <- function(OT_Table_File){
  raw_file <- read.table(file =  OT_Table_File, sep="\t", header=F, fill=T, stringsAsFactors = F, skip=2, quote=NULL) # read in an OTSoft-formatted file, ignoring the first two rows with constaint names
  raw_file[is.na(raw_file)] <- 0 # replace all empty constraint evaluations with 0
  header <- unlist(strsplit(scan(OT_Table_File, what="char", sep="\n")[1], split="\t")) # get the constraint names from the same files
  colnames(raw_file) <- header # add the constraint names as a header to the candidate data frame
  constraint_names <- header[4:length(header)] # get the names of the constraints
  number_of_constraints <- length(constraint_names) # get the number of constraints
  
  cand_freqs <- list() # initialize an empty list object to contain the frequencies of the candidates
  current_tableau <- 0 # initialize a counter
  for(i in which(raw_file[,1] != "")){
    current_tableau <- current_tableau +1
    current_cand_freqs <- c() # initialize an empty vector for candidate frequencies
    current_cand_freqs <- append(current_cand_freqs, raw_file[i,3]) # put the frequency of the first candidate for the current tableaux in the empty vector
    iterator <- i # set an iterator equal to current iterator object i
    while((raw_file[iterator+1,1] == "")){ # as long as the first column in the table is empty (i.e., no new candidate is given), proceed through the candidate frequencies
      current_cand_freqs <- append(current_cand_freqs, raw_file[iterator+1,3]) # append the frequency of the candidate given in column 3
      if(iterator+1 < nrow(raw_file)){ # check that the current line of the table is not at the end of the table
        iterator <- iterator + 1
      }
      else{ # if we have reached the end of the table, exit the while-loop
        break
      }
    }
    cand_freqs[[current_tableau]] <- current_cand_freqs # store the current collected candidate frequencies in the current tableau
  }
  
  violations <- list()
  current_tableau <- 0
  for(i in which(raw_file[,1] != "")){
    number_candidates <- 1
    current_tableau <- current_tableau +1
    current_tableau_violations <- c()
    current_tableau_violations <- append(current_tableau_violations, as.numeric(raw_file[i,4:ncol(raw_file)]))
    iterator <- i
    while((raw_file[iterator+1,1] == "")){
      current_tableau_violations <- append(current_tableau_violations, as.numeric(raw_file[iterator+1,4:ncol(raw_file)]))
      number_candidates <- number_candidates + 1
      if(iterator+1 < nrow(raw_file)){
        iterator <- iterator + 1
      }
      else{
        break
      }
    }
    total_tableau_violations <- matrix(current_tableau_violations, nrow=number_candidates, byrow = T)
    violations[[current_tableau]] <- total_tableau_violations
  }
  
  candidates <- list()
  current_tableau <- 0
  for(i in which(raw_file[,1] != "")){
    current_tableau <- current_tableau +1
    current_candidates <- c()
    current_candidates <- append(current_candidates, raw_file[i,2])
    iterator <- i
    while((raw_file[iterator+1,1] == "")){
      current_candidates <- append(current_candidates, raw_file[iterator+1,2])
      if(iterator+1 < nrow(raw_file)){
        iterator <- iterator + 1
      }
      else{
        break
      }
    }
    candidates[[current_tableau]] <- current_candidates
  }
  
  tableaux <- list(cand_freqs, violations, candidates)
  
  names(tableaux[[1]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
  names(tableaux[[2]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
  names(tableaux[[3]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
  
  candidate_probabilities <- list()
  current_tableau <- 0
  for(i in 1:length(tableaux[[1]])){
    current_tableau <- current_tableau + 1
    current_tableau_probabilities <- c()
    total_token_frequency <- sum(tableaux[[1]][[i]])
    for(j in 1:length(tableaux[[1]][[i]])){
      current_candidate_probability <- tableaux[[1]][[i]][j]/total_token_frequency
      current_tableau_probabilities <- append(current_tableau_probabilities, current_candidate_probability)
    }
    candidate_probabilities[[current_tableau]] <- current_tableau_probabilities
  }
  
  tableaux[[4]] <- candidate_probabilities
  
  
  names(tableaux[[4]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
  tableaux[[5]] <- constraint_names
  
  
  
  
  return(tableaux)
  
}