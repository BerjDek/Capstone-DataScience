#smoothing in case needed to add


# function that adds Laplace smoothing to the frequencies
# NOT USEFUL UNLESS DOING SMOOTHING FOR WHOLE dataframe 
add_laplace_smoothing <- function(ngrams_df) {
  unique_ngrams <- ngrams_df %>%
    group_by(num_words) %>%
    summarise(unique_ngrams = n_distinct(first_gram), .groups = 'drop')
  
  ngrams_df <- ngrams_df %>%
    left_join(unique_ngrams, by = "num_words") %>%
    mutate(smoothed_frequency = (frequency + 1) / (unique_ngrams + n_distinct(next_word)))
  
  return(ngrams_df)
}



new_combined_ngrams <- add_laplace_smoothing(combined_ngrams)
new_df_top_bigrams  <- add_laplace_smoothing(df_top_bigrams)


add_laplace_smoothing <- function(ngrams_df) {
  ngrams_df %>%
    group_by(first_gram) %>%
    mutate(unique_next_words = n_distinct(next_word)) %>%
    ungroup() %>%
    mutate(smoothed_frequency = (frequency + 1) / (sum(frequency) + unique_next_words))
}



new_df_top_bigrams  <- add_laplace_smoothing(df_top_bigrams)
new_df_top_trigrams  <- add_laplace_smoothing(df_top_trigrams)
new_df_top_fourgrams  <- add_laplace_smoothing(df_top_fourgrams)
new_df_top_fivegrams  <- add_laplace_smoothing(df_top_fivegrams)

new_combined_ngrams <- bind_rows(new_df_top_bigrams, new_df_top_trigrams, new_df_top_fourgrams, new_df_top_fivegrams)

##function to work with smoothed combined ngram

smooth_predict_next_words_ngram <- function(input_text, combined_ngrams, top_n = 8, acceptable_words = NULL) {
  words <- str_split(input_text, "\\s+")[[1]]
  results <- data.frame(next_word = character(), smoothed_frequency = numeric(), num_words = integer(), stringsAsFactors = FALSE)
  
  # Function to find matching n-grams
  find_matches <- function(ngrams_df, num_words) {
    last_gram <- paste(tail(words, num_words), collapse = " ")
    matching_ngrams <- ngrams_df %>% filter(first_gram == last_gram)
    if (nrow(matching_ngrams) > 0) {
      next_words <- matching_ngrams %>%
        count(next_word, wt = smoothed_frequency, sort = TRUE) %>%
        mutate(num_words = num_words)
      return(next_words)
    } else {
      return(NULL)
    }
  }
  
  # Check from higher order n-grams to lower order n-grams
  for (num_words in 4:1) {
    ngrams_subset <- combined_ngrams %>% filter(num_words == num_words)
    matches <- find_matches(ngrams_subset, num_words)
    if (!is.null(matches)) {
      results <- bind_rows(results, matches)
    }
  }
  
  # Filter acceptable words after gathering all potential matches
  if (!is.null(acceptable_words)) {
    results <- results %>% filter(next_word %in% acceptable_words)
  }
  
  # Ensure we have exactly top_n results
  results <- results %>% distinct(next_word, .keep_all = TRUE) %>% arrange(desc(num_words), desc(n)) %>% head(top_n)
  
  return(results)
}


acceptable_words <- c("sleep","eat", "die", "give")
smooth_predict_next_words_ngram("i'd live and i'd", new_combined_ngrams, top_n = 20, acceptable_words = acceptable_words)

# Smoothing didnt help as well so keeping it as is, in the future doing the smoothing directly for the combined set instead of the seperate, works just as well

rm(new_df_top_bigrams,new_df_top_fivegrams, new_df_top_fourgrams, new_df_top_trigrams, new_combined_ngrams, add_laplace_smoothing, 
   smooth_predict_next_words_ngram)
