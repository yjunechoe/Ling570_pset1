library(tidyverse) # for data wrangling
library(data.table) # data frame that modifies in-place
library(here) # file referencing
library(tictoc) # for benchmarking

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Prepare Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rollins

rollins_lines <- readLines(here("PS1", "data", "rollins.txt")) %>% 
  discard(~ .x == "") %>% 
  str_remove_all(" #.*$")


rollins <-
  tibble(
    Utterance = 1:(length(rollins_lines)/2),
    Word = rollins_lines[c(TRUE, FALSE)],
    Referent = rollins_lines[c(FALSE, TRUE)]
  ) %>% 
  mutate(across(where(is.character), ~ str_split(.x, " "))) %>% 
  unnest(Word)


rollins_training_lines <- readLines(here("PS1", "data", "rollins.txt.train")) %>% 
  discard(~ .x == "") %>% 
  str_remove_all(" #.*$")

rollins_training <-
  tibble(
    Utterance = 1:(length(rollins_training_lines)/2),
    Word = rollins_training_lines[c(TRUE, FALSE)],
    Referent = rollins_training_lines[c(FALSE, TRUE)]
  ) %>% 
  mutate(across(where(is.character), ~ str_split(.x, " "))) %>% 
  unnest(Word)


# Gold Standard

gold_parsed <- tibble(Line = readLines(here("PS1", "data", "gold.txt"))) %>% 
  separate(Line, c("Word", "Meaning"), " ")



gold_training_parsed <- tibble(Line = readLines(here("PS1", "data", "train.gold"))) %>% 
  separate(Line, c("Word", "Meaning"), " ")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Pursuit ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pursuit <- function(corpus = rollins, gamma = .02, lambda = .001, threshold = .8, sampling = FALSE) {

  tic()
  
  ####################
  ## Pre-processing ##
  ####################
  
  # Grab corpus info
  corpus_words <- sort(unique(corpus$Word))
  n_words <- length(corpus_words)
  WORDS <- setNames(1:n_words, corpus_words)
    
  corpus_referents <- sort(unique(unlist(corpus$Referent)))

  # Store hypotheses and learned meanings
  all_hypotheses <- list()
  learned <- list()
  
  # Vector to check novel words
  seen <- logical(n_words)
  
  # Create association table
  ASSOCIATIONS <- data.table(matrix(0, nrow = n_words, ncol = length(corpus_referents)))
  setnames(ASSOCIATIONS, new = corpus_referents)
  
  # For debugging - track and record hypotheses proposed at each instance
  proposed <- character(nrow(corpus))

  ##############
  ## Learning ##
  ##############
  
  # Run simulation
  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the referents
    word <- corpus$Word[i]
    word_id <- WORDS[word]
    referents <- corpus$Referent[i][[1]]
    
    #######################
    ## 1. Initialization ##
    #######################
    
    # If novel word (no association scores)
    if (!seen[word_id]) {
      
      # Grab referent with lowest association score (randomly sample if multiple)
      association_scores <- as.double(ASSOCIATIONS[, lapply(.SD, max), .SDcols = referents])
      hypothesis <- referents[which.min(association_scores)] #sample(referents[which(association_scores == min(association_scores))], 1)
      
      # Set association score to gamma
      ASSOCIATIONS[[word_id, hypothesis]] <- gamma
      
      # Mark word as seen
      seen[word_id] <- TRUE
      
      # Store hypothesis
      all_hypotheses[[word]][[1]] <- hypothesis
    }
    
    
    ################
    ## 2. Pursuit ##
    ################
    
    # If it is not a novel word
    else{
      
      
      #############################
      ## 2a. Retrieve Hypothesis ##
      #############################
      
      # Retrieve the current hypothesis
      if (length(all_hypotheses[[word]]) == 1) {
        hypothesis <- all_hypotheses[[word]][[1]]
      } else {
        
        # (if multiple hypotheses, sample if `sampling` = TRUE; else retrieve one w/ highest association score)
        if (sampling) {
          cur_hypotheses <- unlist(all_hypotheses[[word]])
          hypotheses_scores <- as.double(ASSOCIATIONS[word_id, ..cur_hypotheses])
          hypotheses_probs <- (hypotheses_scores + lambda)/(sum(hypotheses_scores) + length(corpus_referents) * lambda)
          hypothesis <- sample(cur_hypotheses, 1, prob = hypotheses_probs)
        } else {
          hypothesis <- corpus_referents[which.max(as.double(ASSOCIATIONS[word_id]))]
        }
        
      }
      
      # Retrieve association score for that hypothesis for updating
      hypothesis_score <- ASSOCIATIONS[[word_id, hypothesis]]
      
      
      #########################
      ## 2b. Test Hypothesis ##
      #########################
      
      # If the hypothesis is confirmed, reward it
      if (hypothesis %in% referents) {
        updated_score <- hypothesis_score + gamma * (1 - hypothesis_score)
        ASSOCIATIONS[[word_id, hypothesis]] <- updated_score
        
        # If the meaning hasn't been learned already ...
        if (!hypothesis %in% learned[[word]]) {
          
          # If updated association score passes threshold, add to lexicon
          cur_hypotheses <- unlist(all_hypotheses[[word]])
          denom <- sum(as.double(ASSOCIATIONS[word_id, ..cur_hypotheses])) + length(corpus_referents) * lambda
          
          if ((updated_score + lambda)/denom > threshold) {
            learned[[word]][[length(learned[[word]]) + 1]] <- hypothesis
            
          }
          
        }
        
      }
      
      # If hypothesis disconfirmed
      else {
        
        # Penalize current hypothesis
        ASSOCIATIONS[[word_id, hypothesis]] <- hypothesis_score * (1 - gamma)
        
        # Check if other hypotheses are consistent with referents present
        cur_hypotheses <- unlist(all_hypotheses[[word]])
        consistent_hypotheses <- cur_hypotheses %in% referents
        
        # If so, pick one of the other hypotheses that are consistent
        if (any(consistent_hypotheses)) {
          
          # If multiple are consistent...
          if (sum(consistent_hypotheses) > 1) {
            
            # sample w/ probabilities if `sampling` = TRUE; else sample randomly
            if (sampling) {
              # TODO should I recalculate probabilities AGAIN after penalization? Or just move penalization to the end?
              sampled <- sample(cur_hypotheses[consistent_hypotheses], 1, prob = hypotheses_probs[consistent_hypotheses])
            } else {
              sampled <- sample(cur_hypotheses[consistent_hypotheses], 1)
            }
            
          }
          
          # If there's only one, just pick that one
          else {
            sampled <- cur_hypotheses[consistent_hypotheses]
          }
          
        }
        
        # If none are consistent, randomly sample from referents present
        else {
          
          sampled <- sample(referents, 1)
          
          # Add that to the running list of hypotheses
          all_hypotheses[[word]][[length(all_hypotheses[[word]]) + 1]] <- sampled

        }

        # Reward (or initialize) sampled hypothesis
        hypothesis <- sampled
        sampled_score <- ASSOCIATIONS[[word_id, hypothesis]]
        ASSOCIATIONS[[word_id, hypothesis]] <- sampled_score + gamma * (1 - sampled_score)
        
      }
      
    }
    
    proposed[i] <- hypothesis
    
  }
  
  # ###################
  # ## Normalization ##
  # ###################
  # 
  # # Normalize word-meaning probabilities - Equation 1
  # probabilities <- ASSOCIATIONS %>%
  #   mutate(denom = rowSums(across(everything())) + length(corpus_referents) * lambda) %>%
  #   mutate(across(-denom, ~ (.x + lambda)/denom)) %>%
  #   select(-denom)
  # 
  # 
  # #####################
  # ## Extract Lexicon ##
  # #####################
  #   
  # ## grab meaning with highest probability for each word
  # best_meanings <- max.col(probabilities)
  # 
  # ## grab words that are learned (passing threshold)
  # matrix_indices <- matrix(c(1:n_words, best_meanings), nrow = n_words) 
  # learned_id <- which(as.data.frame(probabilities)[matrix_indices] > threshold)
  # 
  # ## Return the child's lexicon
  # lexicon <- tibble(
  #   Word = names(WORDS)[learned_id],
  #   Learned = corpus_referents[best_meanings[learned_id]]
  # )
  
  toc()
  
  lexicon <- 
    tibble(
      Word = names(learned),
      Learned = map(learned, flatten_chr)
    ) %>% unnest(Learned)
  
  attr(lexicon, "proposed") <- proposed
  
  return(lexicon)
  
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~ Propose but Verify ~~~~~~~~~~~~~~~~~~~~~~~~~~~



PbV <- function(corpus = rollins, alpha0 = .25, alpha = .75) {
  
  memory <- list()
  
  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the referents
    word <- corpus$Word[i]
    referents <- corpus$Referent[i][[1]]
    
    # If new, randomly sample from referents
    if (!word %in% names(memory)) {
      memory[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
    } 
    
    # If heard before
    else {
      
      # If current hypothesis not among referents, randomly sample
      if (!memory[[word]]$Meaning %in% referents) {
        memory[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
      }
      
      # If current hypothesis among referents
      else {
        
        # If it's been confirmed
        if (memory[[word]]$Confirmed) {
          
          # If you fail to retrieve confirmed hypothesis, randomly sample (otherwise leave as is)
          if (runif(1) > alpha) {
            memory[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
          }
          
        }
        
        # If it's not been confirmed
        else {
          
          # If you fail to retrieve unconfirmed hypothesis, randomly sample
          if (runif(1) < alpha0) {
            memory[[word]]$Meaning <- sample(referents, 1)
          }
          
          # If you correctly think it's been unconfirmed, confirm it
          else {
            memory[[word]]$Confirmed <- TRUE
          }
          
        }
        
      }
      
    }
    
  }

  result <- tibble(
    Word = names(memory),
    Learned = map_chr(memory, 1),
  )
  
  result[map_lgl(memory, 2), ]
  
}







# ~~~~~~~~~~~~~~~~~~~~~~~~~ Cross Situational ~~~~~~~~~~~~~~~~~~~~~~~~~~~


xsit <- function(corpus = rollins, beta = 100, lambda = .001, threshold = .8, sampling = FALSE) {
  
  tic()
  
  ####################
  ## Pre-processing ##
  ####################
  
  # Grab corpus info
  corpus_words <- sort(unique(corpus$Word))
  corpus_referents <- sort(unique(unlist(corpus$Referent)))
  REFERENTS <- setNames(1:length(corpus_referents), corpus_referents)
  
  # List to store learned meanings
  learned <- list()
  
  # Initialize probability table with equal likelihood, row sums to 1
  ALIGNMENTS <- data.table(matrix(1/length(corpus_words), nrow = length(corpus_referents), ncol = length(corpus_words)))
  setnames(ALIGNMENTS, new = corpus_words)


  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the referents
    word <- corpus$Word[i]
    referents <- corpus$Referent[i][[1]]
    referents_id <- REFERENTS[referents]
    
    
    ######################
    ## Alignments(w, m) ##
    ######################
    
    # Retrieve probability distribution of meanings over the word
    word_probabilities <- ALIGNMENTS[[word]]
    
    # Calculate alignments for referents present
    alignments <- word_probabilities[referents_id]/sum(word_probabilities[referents_id])
    
    # Update alignment table
    word_probabilities[referents_id] <- alignments
    ALIGNMENTS[[word]] <- word_probabilities
    
    # Normalize probabilities within each meaning (rows)
    for (referent_id in referents_id) {
      ALIGNMENTS[referent_id] <- ALIGNMENTS[referent_id]/sum(as.double(ALIGNMENTS[referent_id]))
    }
    
    
    #TODO
    
    ########################
    ## Probabilities(w|m) ##
    ########################
    
    # Normalize probabilities within the word (column)
    
    
    if (max(PROBABILITIES[[word]]) > threshold) {
      meaning <- names(REFERENTS)[which.max(updated_word_probabilities)]
      # If it's new, add it to the lexicon
      if (!meaning %in% unlist(learned[[word]])) {
        learned[[word]][[length(learned[[word]]) + 1]] <- meaning
      }
    }
    
  }
  
  learned
  
    
}






# Evaluation Algorithm

eval_algo <- function(result){
  
  performance <- full_join(result, gold_parsed, by = "Word")
  
  true_positives <- nrow(filter(performance, Learned == Meaning))
  
  # Of those words learned, which were learned correctly?
  precision <- true_positives/nrow(result)
  
  # Of the words that should be learned, which were learned correctly?
  recall <- true_positives/nrow(gold_parsed)
  
  F1 <- 2 * (precision * recall)/(precision + recall)
  
  tibble(
    Precision = precision,
    Recall = recall,
    F1 = F1
  )
  
}


