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
library(tictoc); library(microbenchmark) # for benchmarking
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


####### Manual load
## rollins_training <- readRDS("rollins_training.rds")
## gold_parsed <- read_csv("gold_training_parsed.csv")
#######



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
      hypothesis <- sample(referents[which(association_scores == min(association_scores))], 1) # referents[which.min(association_scores)]
      
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
      
      # Grab association score for that hypothesis for updating
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
        
        # Sample randomly from referents present
        selected <- sample(referents, 1)
        
        # Reward (or initialize) selected hypothesis
        selected_score <- ASSOCIATIONS[[word_id, selected]]
        set(ASSOCIATIONS, word_id, selected, selected_score + gamma * (1 - selected_score))
        
        # Add it to the running list of hypotheses
        all_hypotheses[[word]][[length(all_hypotheses[[word]]) + 1]] <- selected
        
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
  # result[map_lgl(meanings, 2), ]
  
  result
  
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~ Cross Situational ~~~~~~~~~~~~~~~~~~~~~~~~~~~


xsit <- function(corpus = rollins_training, beta = 100, lambda = .01, threshold = .09) {
  
  tic()
  
  ####################
  ## Pre-processing ##
  ####################
  
  # Add dummy referent to corpus
  corpus <- corpus %>% 
    mutate(Referent = map(Referent, ~c(.x, "dummy")))
  
  # Grab corpus info
  corpus_words <- sort(unique(corpus$Word))
  corpus_referents <- sort(unique(unlist(corpus$Referent)))
  WORDS <- setNames(1:length(corpus_words), corpus_words)
  
  # Restructure corpus by utterance
  corpus <- corpus %>% 
    group_by(Utterance, Referent) %>% 
    nest(Word = Word) %>% 
    ungroup() %>% 
    mutate(Word = map(Word, ~.x$Word))
  
  # List to store learned meanings
  learned <- list()
  
  # Vector to check novel words
  seen <- logical(length(corpus_words))
  
  # Initialize alignment probability tables
  PROBABILITIES <- data.table(matrix(0, nrow = length(corpus_words), ncol = length(corpus_referents)))
  setnames(PROBABILITIES, new = corpus_referents)
  ALIGNMENTS <- data.table(matrix(0, nrow = length(corpus_words), ncol = length(corpus_referents)))
  setnames(ALIGNMENTS, new = corpus_referents)

  # For each utterance in corpus
  for (i in 1:nrow(corpus)) {
    
    # Grab the words and the referents
    words <- corpus$Word[i][[1]]
    referents <- corpus$Referent[i][[1]]

    for (word in words) {
      word_id <- WORDS[word]
      
      for (referent in referents) {
        
        # Initialize
        if (PROBABILITIES[[word_id, referent]] == 0) {
          set(PROBABILITIES, word_id, referent, 1/beta)
          seen[word_id] <- TRUE
        }
        
      }
      
    }
    
    for(referent in referents) {
      
      # Adjust alignments
      for (word in words) {
        word_id <- WORDS[word]
        
        alignment_adjustment <- PROBABILITIES[[word_id, referent]]/(sum(PROBABILITIES[word_id, ..referents]))
        set(ALIGNMENTS, word_id, referent, ALIGNMENTS[[word_id, referent]] + alignment_adjustment)
      }
      
    }
    
    for (referent in referents) {

      # Adjust probabilities
      for (word in WORDS[seen]) {
        word_id <- WORDS[word]
        
        probability_adjustment <- (ALIGNMENTS[[word_id, referent]] + lambda)/(sum(ALIGNMENTS[[referent]]) + (beta * lambda))
        set(PROBABILITIES, word_id, referent, probability_adjustment)
      }
      
    }
    
  }
  
  ## grab meaning with highest probability for each word
  best_meanings <- max.col(PROBABILITIES)
  
  ## grab words that are learned (passing threshold)
  matrix_indices <- matrix(c(1:length(corpus_words), best_meanings), nrow = length(corpus_words))
  learned_id <- which(as.data.frame(PROBABILITIES)[matrix_indices] > threshold)
  
  
  ## Return the child's lexicon
  lexicon <- tibble(
    Word = names(WORDS)[learned_id],
    Learned = corpus_referents[best_meanings[learned_id]]
  )
  
  attr(lexicon, "align_table") <- ALIGNMENTS
  attr(lexicon, "prob_table") <- PROBABILITIES
  
  toc()
  
  return(lexicon)
  
}





# ~~~~~~~~~~~~~~~~~~~~~~~ Evaluation Functions ~~~~~~~~~~~~~~~~~~~~~~~~



# Evaluation Algorithm

eval_algo <- function(result, standard = gold_parsed){
  
  performance <- full_join(result, standard, by = "Word")
  
  true_positives <- nrow(filter(performance, Learned == Meaning))
  
  # Of those words learned, which were learned correctly?
  precision <- true_positives/nrow(result)
  
  # Of the words that should be learned, which were learned correctly?
  recall <- true_positives/nrow(standard)
  
  F1 <- 2 * (precision * recall)/(precision + recall)
  
  tibble(
    Precision = precision,
    Recall = recall,
    F1 = F1
  )
  
}


# Evaluate multiple simulations

eval_sims <- function(sims, ...) {
  sims %>% 
    future_map_dfr(eval_algo, ...) %>% 
    summarize_all(mean)
}




# ~~~~~~~~~~~~~~~~~~~~~~~ Run Simulations ~~~~~~~~~~~~~~~~~~~~~~~~


plan(multisession, workers = availableCores() - 1)

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


# PbV
with_progress({
  PbV_sims <- run_sim(PbV, 1000)
  PbV_sims_metrics <- eval_sims(PbV_sims)
})

# pursuit
with_progress({
  pursuit_sims <- run_sim(pursuit, 1000)
  pursuit_sims_metrics <- eval_sims(pursuit_sims)
})

# pursuit w/ sampling
with_progress({
  pursuit_sampling_sims <- run_sim(pursuit, 1000, sampling = TRUE)
  pursuit_sampling_sims_metrics <- eval_sims(pursuit_sampling_sims)
})

xsit_sims <- xsit()
xsit_sims_metrics <- eval_algo(xsit_sims)




# ~~~~~~~~~~~~~~~~~~~~~~~ Build Table ~~~~~~~~~~~~~~~~~~~~~~~~

library(kableExtra)

F1_list <- list(
  future_map_dfr(PbV_sims, eval_algo)$F1,
  future_map_dfr(pursuit_sims, eval_algo)$F1,
  future_map_dfr(pursuit_sampling_sims, eval_algo)$F1,
  xsit_sims_metrics$F1
)

tbl_df <- bind_rows(
  PbV_sims_metrics,
  pursuit_sims_metrics,
  pursuit_sampling_sims_metrics,
  xsit_sims_metrics
)

tbl_df %>% 
  mutate(Model = c("Propose but Verify", "Pursuit", "Pursuit (sampling)", "Cross-situational")) %>% 
  relocate(Model) %>% 
  mutate(
    `F1-Dist` = "",
    across(where(is.double), ~round(.x, 3))
  ) %>% 
  kbl() %>% 
  kable_classic(full_width = FALSE) %>% 
  add_header_above(c("", "Performance Metrics" = 4)) %>% 
  column_spec(2, background = ifelse(tbl_df[[1]] == max(tbl_df[[1]]), "#ADADADFF", "white")) %>% 
  column_spec(3, background = ifelse(tbl_df[[2]] == max(tbl_df[[2]]), "#ADADADFF", "white")) %>% 
  column_spec(4, background = ifelse(tbl_df[[3]] == max(tbl_df[[3]]), "#ADADADFF", "white")) %>% 
  column_spec(5, image = spec_hist(F1_list, breaks = c(5, 10, 10, 1), col = c("grey", "grey", "grey", NA), border = c("black", "black", "black", NA))) %>%
  kable_styling(
    bootstrap_options = "none", 
    font_size = 18,
    html_font = "CMU Typewriter Text"
  ) %>% 
  as_image(file = "performance_tbl.png")

