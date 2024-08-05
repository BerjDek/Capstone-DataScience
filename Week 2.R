# Install necessary packages if not already installed
install.packages("tidyverse")
install.packages("shiny")
install.packages("knitr")
install.packages("stringr")
install.packages("tm")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("textdata")
install.packages("reshape2")

rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(shiny)
library(knitr)
library(stringr)
library(tidytext)
library(wordcloud)
library(textdata)
library(reshape2)

# Load the data
blogs <- readLines("en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
twitter <- readLines("en_US.twitter.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("en_US.news.txt", warn = FALSE, encoding = "UTF-8")

# Check the data
head(blogs)
head(twitter)
head(news)

library(tibble)
# Create summaries of each
blogs_summary <- tibble(
  File = "Blogs",
  Characters = sum(nchar(blogs)),
  Lines = length(blogs),
  Words = sum(str_count(blogs, "\\w+")),
  Size_MB = file.size("en_US.blogs.txt") / (1024^2)
)

twitter_summary <- tibble(
  File = "Twitter",
  Characters = sum(nchar(twitter)),
  Lines = length(twitter),
  Words = sum(str_count(twitter, "\\w+")),
  Size_MB = file.size("en_US.twitter.txt") / (1024^2)
)

news_summary <- tibble(
  File = "News",
  Characters = sum(nchar(news)),
  Lines = length(news),
  Words = sum(str_count(news, "\\w+")),
  Size_MB = file.size("en_US.news.txt") / (1024^2)
)

library(dplyr)
# Combine all summaries into one table
summary_stats <- bind_rows(blogs_summary, twitter_summary, news_summary)



library(knitr)
kable(summary_stats, caption = "Summary Statistics of Text Data")


# Sample the data to create manageable datasets for plotting, choosing 1000 lines from each
set.seed(123)
blogs_sample <- sample(blogs, 5000)
twitter_sample <- sample(twitter, 5000)
news_sample <- sample(news, 5000)

text_samples <- c(blogs_sample, twitter_sample, news_sample)

# Create a corpus
corpus <- Corpus(VectorSource(text_samples))

# Clean the corpus
# Load a list of profanities in English
profanity <- read.csv("profanity_en.csv", header = FALSE, stringsAsFactors = FALSE)
profanity <- profanity$V1

corpus_clean <- tm_map(corpus, content_transformer(tolower)) # Convert text to lower case
corpus_clean <- tm_map(corpus_clean, removePunctuation) # Remove punctuations
corpus_clean <- tm_map(corpus_clean, removeNumbers) # Remove numbers
corpus_clean <- tm_map(corpus_clean, stripWhitespace) # Remove extra whitespaces
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("en")) # Remove common stopwords
corpus_clean <- tm_map(corpus_clean, removeWords, profanity) # Remove profanities

# Remove URLs
remove_url <- content_transformer(function(x) gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", x))
corpus_clean <- tm_map(corpus_clean, remove_url)

# Convert cleaned corpus to a data frame
corpus_df <- data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE)

# Tokenize into unigrams
unigrams <- corpus_df %>%
  unnest_tokens(word, text)

# Calculate unigram frequencies
unigram_freq <- unigrams %>%
  count(word, sort = TRUE)

# Tokenize into bigrams
bigrams <- corpus_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))  # Remove NA values

# Calculate bigram frequencies
bigram_freq <- bigrams %>%
  count(bigram, sort = TRUE)

# Tokenize into trigrams
trigrams <- corpus_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram))  # Remove NA values

# Calculate trigram frequencies
trigram_freq <- trigrams %>%
  count(trigram, sort = TRUE)

# Display the top 10 unigrams, bigrams, and trigrams
print("Top 10 Unigrams")
print(head(unigram_freq, 10))

top30_unigrams <- unigram_freq %>% top_n(30, wt = n)
ggplot(top30_unigrams, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 30 Unigrams", x = "Unigrams", y = "Frequency") +
  theme_minimal()


print("Top 10 Bigrams")
print(head(bigram_freq, 10))

top30_bigrams <- bigram_freq %>% top_n(30, wt = n)

ggplot(top30_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 30 Bigrams", x = "Bigrams", y = "Frequency") +
  theme_minimal()


print("Top 10 Trigrams")
print(head(trigram_freq, 10))

top20_trigrams <- trigram_freq %>% top_n(20, wt = n)

ggplot(top20_trigrams, aes(x = reorder(trigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 20 Trigrams", x = "Trigrams", y = "Frequency") +
  theme_minimal()



# Word Cloud
set.seed(1234)
wordcloud(words = unigram_freq$word, freq = unigram_freq$n, min.freq = 50,
          max.words = 200, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Sentiment Analysis
nrc_sentiments <- get_sentiments("nrc")

sentiment_scores <- unigrams %>%
  inner_join(nrc_sentiments, by = "word") %>%
  count(sentiment, sort = TRUE)

ggplot(sentiment_scores, aes(x = reorder(sentiment, n), y = n)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Sentiment Analysis", x = "Sentiment", y = "Frequency") +
  theme_minimal()

