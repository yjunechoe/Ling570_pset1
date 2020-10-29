###############
##
##  June Choe
##  Ling 570
##  PSET #1
##
###############


library(tidyverse) # for data wrangling
library(data.table) # for fast modify-in-place
library(furrr) # for parallel computing
library(here) # for file referencing
library(tictoc) # for benchmarking
library(progressr) # for progress bar


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


pursuit <- function(corpus = rollins_training, gamma = .02, lambda = .001, threshold = .78, sampling = FALSE) {

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
      set(ASSOCIATIONS, word_id, hypothesis, gamma)
      
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
        set(ASSOCIATIONS, word_id, hypothesis, hypothesis_score + gamma * (1 - hypothesis_score))
      }
      
      # If hypothesis disconfirmed
      else {
        
        # Penalize current hypothesis
        set(ASSOCIATIONS, word_id, hypothesis, hypothesis_score * (1 - gamma))
        
        # If it's pursuit w/ sampling AND any of the other hypotheses are confirmed
        if(sampling && any(unlist(all_hypotheses[[word]]) %in% referents)) {
          
          # Retrieve those that are consistent with referents present
          consistent_hypotheses <- unlist(all_hypotheses[[word]]) %in% referents
          
          # If multiple are consistent...
          if (sum(consistent_hypotheses) > 1) {
            
            # sample w/ probabilities
            # TODO should I recalculate probabilities AGAIN after penalization? Or just move penalization to the end?
            selected <- sample(cur_hypotheses[consistent_hypotheses], 1, prob = hypotheses_probs[consistent_hypotheses])
          }
          
          # If there's only one, just pick that one
          else {
            selected <- cur_hypotheses[consistent_hypotheses]
          }
          
        }
        
        else {
          # Sample randomly from referents present
          selected <- sample(referents, 1)
          
          # Add that to the running list of hypotheses
          all_hypotheses[[word]][[length(all_hypotheses[[word]]) + 1]] <- selected  
        }
        
        # Reward (or initialize) selected hypothesis
        new_hypothesis <- selected
        selected_score <- ASSOCIATIONS[[word_id, new_hypothesis]]
        
        set(ASSOCIATIONS, word_id, new_hypothesis, selected_score + gamma * (1 - selected_score))
        
      }
      
    }
    
    proposed[i] <- hypothesis
    
  }
  
  ###################
  ## Normalization ##
  ###################

  # Normalize word-meaning probabilities - Equation 1
  probabilities <- ASSOCIATIONS %>%
    mutate(denom = rowSums(across(everything())) + length(corpus_referents) * lambda) %>%
    mutate(across(-denom, ~ (.x + lambda)/denom)) %>%
    select(-denom)


  #####################
  ## Extract Lexicon ##
  #####################

  ## grab meaning with highest probability for each word
  best_meanings <- max.col(probabilities)

  ## grab words that are learned (passing threshold)
  matrix_indices <- matrix(c(1:n_words, best_meanings), nrow = n_words)
  learned_id <- which(as.data.frame(probabilities)[matrix_indices] > threshold)

  ## Return the child's lexicon
  lexicon <- tibble(
    Word = names(WORDS)[learned_id],
    Learned = corpus_referents[best_meanings[learned_id]]
  )
  
  attr(lexicon, "hypotheses") <- all_hypotheses
  attr(lexicon, "proposed") <- proposed
  attr(lexicon, "assoc_table") <- ASSOCIATIONS
  
  return(lexicon)
  
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~ Propose but Verify ~~~~~~~~~~~~~~~~~~~~~~~~~~~



PbV <- function(corpus = rollins_training, alpha0 = 0, alpha = 1) {
  
  # Create empty list for storing hypotheses
  meanings <- list()
  
  # Incrementally loop through each word in the corpus
  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the co-present referents
    word <- corpus$Word[i]
    referents <- corpus$Referent[i][[1]]
    
    # First time seeing the word?
    if (!word %in% names(meanings)) {
      # Randomly pick a hypothesis from the referents present
      meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
    } 
    
    else {
      
      # Current hypothesis not among the referents?
      if (!meanings[[word]]$Meaning %in% referents) {
        # Randomly pick a hypothesis from the referents present
        meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
      }
      
      else {
        
        # Current hypothesis already been confirmed?
        if (meanings[[word]]$Confirmed) {
          
          # Fail to retrieve confirmed hypothesis?
          if (runif(1) > alpha) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
          }
          
          else{
            # Do nothing
          }
          
        }
        
        else {
          
          # Fail to retrieve the unconfirmed hypothesis?
          if (runif(1) < alpha0) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]]$Meaning <- sample(referents, 1)
          }
          
          else {
            # Confirm the current hypothesis
            meanings[[word]]$Confirmed <- TRUE
          }
          
        }
        
      }
      
    }
    
  }

  # A dataframe of all hypotheses proposed
  result <- tibble(
    Word = names(meanings),
    Learned = map_chr(meanings, 1)
  )
  
  # Return only those hypotheses that have been confirmed (= learned)
  result[map_lgl(meanings, 2), ]
  
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



# ~~~~~~~~~~~~~~~~~~~~~~~ Evaluation Functions ~~~~~~~~~~~~~~~~~~~~~~~~



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


# Evaluate multiple simulations

eval_sims <- function(sims) {
  sims %>% 
    future_map_dfr(eval_algo) %>% 
    summarize_all(mean)
}



# ~~~~~~~~~~~~~~~~~~~~~~~ Run Simulations ~~~~~~~~~~~~~~~~~~~~~~~~


plan(multisession, workers = 4)

run_sim <- function(model, n = 100, ...) {
  tic()
  
  params <- list(...)
  
  p <- progressor(steps = n)

  sims <- future_map(
    1:n,
    ~{
      p()
      do.call(model, params)
    },
    .options = furrr_options(seed = TRUE)
  )
  
  toc()
  
  sims
}





with_progress({
  PbV_sims <- run_sim(PbV, 1000)
})


with_progress({
  pursuit_sims <- run_sim(pursuit, 1000)
})





# ~~~~~~~~~~~~~~~~~~~~~~~ Build Table ~~~~~~~~~~~~~~~~~~~~~~~~




F1_list <- list(
  PbV_sims_raw$F1,
  pursuit_sims_raw$F1
)


bind_rows(
  PbV_sims_raw %>% 
    summarize_all(mean) %>% 
    mutate(Model = "Propose but Verify"),
  pursuit_sims_raw %>% 
    summarize_all(mean) %>% 
    mutate(Model = "Pursuit")
) %>% 
  relocate(Model) %>% 
  mutate(
    `F1-Distribution` = "",
    across(where(is.double), ~round(.x, 3))
  ) %>% 
  kbl() %>% 
  kable_classic(full_width = FALSE) %>% 
  add_header_above(c("", "Performance Metrics" = 4)) %>% 
  column_spec(5, image = spec_boxplot(F1_list)) %>%
  kable_styling(bootstrap_options = "none", font_size = 12, html_font = "CMU Typewriter Text")
  

