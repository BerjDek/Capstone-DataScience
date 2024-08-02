library(quanteda)
library(stringr)
library(dplyr)
library(tidytext)



# Load and Sample Data (for initial time do this, if already done before load automatically the text sample from below)
set.seed(123)
blogs_sample <- sample(readLines("en_US.blogs.txt", warn = FALSE, encoding = "UTF-8"), 400000)
twitter_sample <- sample(readLines("en_US.twitter.txt", warn = FALSE, encoding = "UTF-8"), 800000)
news_sample <- sample(readLines("en_US.news.txt", warn = FALSE, encoding = "UTF-8"), 50000)
text_samples <- c(blogs_sample, twitter_sample, news_sample)


text_samples <- gsub("[$>=<`|£⇨─®€+~█▒☺♡♥¦♦♫â✌✔❒❕⚡°^�⚾]", "", text_samples) # for some reason these symbols were stubborn and wouldn't be 
#cleaned up with  tokens_remove(pattern = "[$>]+") added below when createing mycorpus_tokens
text_samples <- gsub("#\\w+", "", text_samples)  #removes hashtaged words from twitter
text_samples <- gsub("\\b\\d+(th|st|nd|rd|s|am|pm|hdc|k|hdc)\\b", "", text_samples) #removes numbers for ranking or dates like 1800s
#save to load faster later
saveRDS(text_samples, file = "text_samples.rds")


# Load the text_samples object
#text_samples <- load("text_samples.rds")




# Load the banned words
banned_words <- read.csv("profanity_en.csv", stringsAsFactors = FALSE)$text

myCorpus <- corpus(text_samples)


mycorpus_tokens <- tokens(myCorpus,
                          remove_numbers = TRUE,
                          remove_punct = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE,
                          split_hyphens = FALSE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = banned_words) 

saveRDS(mycorpus_tokens, file = "mycorpus_tokens.rds")

# Create bi-tri-quad-five-grams
bigrams <- tokens_ngrams(mycorpus_tokens, n = 2)
trigrams <- tokens_ngrams(mycorpus_tokens, n = 3)
fourgrams <- tokens_ngrams(mycorpus_tokens, n = 4)
fivegrams <- tokens_ngrams(mycorpus_tokens, n = 5)


saveRDS(bigrams, file = "bigrams.rds")
saveRDS(trigrams, file = "trigrams.rds")
saveRDS(fourgrams, file = "fourgrams.rds")
saveRDS(fivegrams, file = "fivegrams.rds")

#clearing up unnecesary items from environment

rm(mycorpus_tokens,banned_words,blogs_sample,myCorpus,news_sample, text_samples, twitter_sample)



#if you start from here load the gram objects
#bigrams <- readRDS("bigrams.rds")
# trigrams <- readRDS("trigrams.rds")
# fourgrams <- readRDS("fourgrams.rds")
# fivegrams <- readRDS("fivegrams.rds")



#function to preprocess created n-gram dataframes
preprocess_ngrams <- function(ngrams_df, n) {
  ngrams_df %>%
    mutate(
      term = str_replace_all(term, "_", " "),
      first_gram = word(term, 1, n-1),
      next_word = word(term, n),
      num_words = n - 1
    ) %>%
    select(first_gram, next_word, frequency, num_words)
}


#load items that are already saved
df_top_bigrams <- readRDS("df_top_bigrams_300K.rds")


# Extract top 100 most frequent bigrams
dfm_bigrams <- dfm(bigrams)
saveRDS(dfm_bigrams, file = "dfm_bigrams.rds")

top_bigrams <- topfeatures(dfm_bigrams, 300000)
df_top_bigrams <- data.frame(term = names(top_bigrams), frequency = unname(top_bigrams))

# Preprocess the dataframe
df_top_bigrams <- preprocess_ngrams(df_top_bigrams, 2)  
# the lowest frequency with 300k top bigrams selected for bigrams is 8

saveRDS(df_top_bigrams, file = "df_top_bigrams_300K.rds")

rm(bigrams, top_bigrams, dfm_bigrams)



# Extract top 100 most frequent trigrams and repeat the steps

dfm_trigrams <- dfm(trigrams)
saveRDS(dfm_trigrams, file = "dfm_trigrams.rds")

top_trigrams <- topfeatures(dfm_trigrams, 300000)
df_top_trigrams <- data.frame(term = names(top_trigrams), frequency = unname(top_trigrams))

df_top_trigrams <- preprocess_ngrams(df_top_trigrams, 3)

saveRDS(df_top_trigrams, file = "df_top_trigrams_300K.rds")

rm(trigrams, top_trigrams, dfm_trigrams)


# Extract top 100 most frequent fourgrams
dfm_fourgrams <- dfm(fourgrams)
saveRDS(dfm_fourgrams, file = "dfm_fourgrams.rds")

top_fourgrams <- topfeatures(dfm_fourgrams, 100000)
df_top_fourgrams <- data.frame(term = names(top_fourgrams), frequency = unname(top_fourgrams))

df_top_fourgrams <- preprocess_ngrams(df_top_fourgrams, 4)

saveRDS(df_top_fourgrams, file = "df_top_fourgrams_100K.rds")

rm(fourgrams, top_fourgrams, dfm_fourgrams)



# Extract top 100 most frequent fivegrams
dfm_fivegrams <- dfm(fivegrams)
saveRDS(dfm_fivegrams, file = "dfm_fivegrams.rds")

top_fivegrams <- topfeatures(dfm_fivegrams, 100000)
df_top_fivegrams <- data.frame(term = names(top_fivegrams), frequency = unname(top_fivegrams))


df_top_fivegrams <- preprocess_ngrams(df_top_fivegrams, 5)

saveRDS(df_top_fivegrams, file = "df_top_fivegrams_100K.rds")

rm(fivegrams, top_fivegrams, dfm_fivegrams)


#create a function that adds Laplace smoothing


add_laplace_smoothing <- function(ngrams_df) {
  unique_ngrams <- ngrams_df %>%
    group_by(num_words) %>%
    summarise(unique_ngrams = n_distinct(first_gram), .groups = 'drop')
  
  ngrams_df <- ngrams_df %>%
    left_join(unique_ngrams, by = "num_words") %>%
    mutate(smoothed_frequency = (frequency + 1) / (unique_ngrams + n_distinct(next_word)))
  
  return(ngrams_df)
}


#smooth the ngram dataframes
df_top_bigrams_s  <- add_laplace_smoothing(df_top_bigrams)
df_top_trigrams_s <- add_laplace_smoothing(df_top_trigrams)
df_top_fourgrams_s  <- add_laplace_smoothing(df_top_fourgrams)
df_top_fivegrams_s  <- add_laplace_smoothing(df_top_fivegrams)


combined_ngramss <- bind_rows(df_top_bigrams, df_top_trigrams, df_top_fourgrams, df_top_fivegrams)

#comining Ngram dataframes
combined_ngrams <- bind_rows(df_top_bigrams_s, df_top_trigrams_s, df_top_fourgrams_s, df_top_fivegrams_s)

write.csv(combined_ngrams, file = "combined_ngrams_800.csv", row.names = FALSE)



#creating a function that uses the combined data frames to predict the next word prioritizing longer ngrams.
#the function takes the input text converts converts it to lowertext, splits it into individual words, and seraches for matching n-grams starting from
#the highest (5) to lower order of n-grams. for each match it uses smoothed frequencies to  predictions and collects potential next words along with 
#their original frequencies and the number of words in the n-gram. it optionally filters them based on a list of acceptable words if its given one.
# Finally, it returns the top results sorted by the number of words in the n-gram and their original frequencies, providing a ranked list of next 
#word predictions.
 

predict_next_words_ngram <- function(input_text, combined_ngrams, top_n = 8, acceptable_words = NULL) {
  input_text <- tolower(input_text)
  words <- str_split(input_text, "\\s+")[[1]]
  results <- data.frame(next_word = character(), frequency = integer(), num_words = integer(), stringsAsFactors = FALSE)
  
  # Function to find matching n-grams
  find_matches <- function(ngrams_df, num_words) {
    last_gram <- paste(tail(words, num_words), collapse = " ")
    matching_ngrams <- ngrams_df %>% filter(first_gram == last_gram)
    if (nrow(matching_ngrams) > 0) {
      next_words <- matching_ngrams %>%
        arrange(desc(smoothed_frequency)) %>%
        select(next_word, frequency, num_words)
      return(next_words)
    } else {
      return(NULL)
    }
  }
  
  # Check from higher order n-grams to lower order n-grams
  for (num_words in 5:1) { # Update to include 5-grams
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
  results <- results %>% distinct(next_word, .keep_all = TRUE) %>% arrange(desc(num_words), desc(frequency)) %>% head(top_n)
  
  return(results)
}


acceptable_words <- c("eat", "give", "die", "sleep", "marital", "financial", "horticultural", "spiritual", 
                      "weekend", "morning", "decade", "month", "happiness", "stress", "hunger", "sleepiness", 
                      "walk", "look", "minute", "picture", "case", "incident", "account", "matter", "toe", "hand", 
                      "finger", "arm", "middle", "center", "side", "top", "inside", "weekly", "outside", "daily", 
                      "movies", "stories", "pictures", "novels", "see", 
                      "marital","financial","horticultural","spiritual")

acceptable_words <- c("case","matter", "account", "incident")






#example
predict_next_words_ngram("a jury to settle the", combined_ngrams, top_n = 20, acceptable_words = acceptable_words)



   