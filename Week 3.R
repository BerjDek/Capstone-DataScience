
# Install necessary packages if not already installed
install.packages(c("tm", "dplyr", "tidyr", "tidytext", "stringr", "purrr"))

# Load necessary libraries
library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(purrr)




# Load the data
blogs <- readLines("en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
twitter <- readLines("en_US.twitter.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("en_US.news.txt", warn = FALSE, encoding = "UTF-8")



set.seed(123)
blogs_sample <- sample(blogs, 30000)
twitter_sample <- sample(twitter, 30000)
news_sample <- sample(news, 30000)

text_samples <- c(blogs_sample, twitter_sample, news_sample)

# Create a corpus
corpus <- Corpus(VectorSource(text_samples))


#note when cleaning unlike last time we do not remove stopwords and cursewords


corpus_clean <- tm_map(corpus, content_transformer(tolower)) # Convert text to lower case
corpus_clean <- tm_map(corpus_clean, removePunctuation) # Remove punctuations
corpus_clean <- tm_map(corpus_clean, removeNumbers) # Remove numbers
corpus_clean <- tm_map(corpus_clean, stripWhitespace) # Remove extra whitespaces
corpus_clean <- tm_map(corpus_clean, removeWords, profanity) # Remove profanities


remove_url <- content_transformer(function(x) gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", x))  # Remove URLs
corpus_clean <- tm_map(corpus_clean, remove_url)

rm(remove_url)


# Convert cleaned corpus to a data frame
corpus_df <- data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE)



# Tokenize into trigrams
trigrams <- corpus_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram))




stopwords <- c("the", "of", "and", "a", "to", "in", "is", "you", "that", "it", 
               "he", "was", "for", "on", "are", "as", "with", "his", "they", 
               "I", "at", "be", "this", "have", "from", "or", "one", "had", 
               "by", "word", "but", "not", "what", "all", "were", "we", "when", 
               "your", "can", "said", "there", "use", "an", "each", "which", 
               "she", "do", "how", "their", "if", "also", "will")


# Function to predict the top 10 next words
predict_next_words <- function(input_text, trigrams_df, top_n = 200) { 
  words <- str_split(input_text, "\\s+")[[1]]                           # Extract the last bigram from the input text
  last_bigram <- paste(tail(words, 2), collapse = " ")
  matching_trigrams <- trigrams_df %>%                                 # Find matching trigrams
    filter(str_starts(trigram, last_bigram))
  if (nrow(matching_trigrams) > 0) {
    next_words <- matching_trigrams %>%                               # Extract the next words from matching trigrams
      mutate(next_word = str_split(trigram, " ") %>% map_chr(3)) %>%
      count(next_word, sort = TRUE) %>%
      filter(!next_word %in% stopwords) %>%
      top_n(n = top_n, wt = n)
    return(next_words)
  } else {
    return(NA)
  }
}




#Fragment 1

predict_next_words("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", trigrams) %>%
  filter(next_word %in%  c("beer", "soda", "cheese", "pretzels"))#beer


#Fragment 2
predict_next_words("You're the reason why I smile everyday. Can you follow me please? It would mean the", trigrams) %>%
  filter(next_word %in%  c("world", "best", "universe", "most")) #world

#Fragment 3
predict_next_words("Hey sunshine, can you follow me and make me", trigrams) %>%
  filter(next_word %in%  c("happiest", "smelliest", "saddest", "bluest"))  #NA going with happy after deleting the WAS correct
 

#Fragment 4
predict_next_words("Very early observations on the Bills game: Offense still struggling but the", trigrams)%>%
  filter(next_word %in%  c("crowd", "referees", "defense", "players"))  #NA  

#Fragment 5
predict_next_words("Go on a romantic date at the", trigrams)  %>%
  filter(next_word %in%  c("beach", "mall", "movies", "grocery"))  #beach

#Fragment 6
predict_next_words("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", trigrams)%>%
  filter(next_word %in%  c("phone", "way", "motorcycle", "horse"))  #way

#Fragment 7
predict_next_words("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", trigrams)  #time

#Fragment 8
predict_next_words("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", trigrams) %>%
  filter(next_word %in%  c("fingers", "eyes", "toes", "ears"))  #eyes WRONG

#Fragment 9
predict_next_words("Be grateful for the good times and keep the faith during the", trigrams)%>%
  filter(next_word %in%  c("worse", "sad", "bad", "hard"))  #NA CHOSE SAD CORRECT

#Fragment 10
predict_next_words("If this isn't the cutest thing you've ever seen, then you must be", trigrams)%>%
  filter(next_word %in%  c("insane", "asleep", "insensitive", "callous"))  #insane

