library(tidyverse)


#Loading Data
data_X <- read_tsv("C:/Users/BerjD/Desktop/Capstone/Capstone-DataScience/en_US.twitter.txt", col_names = FALSE)
data_news <- read_tsv("C:/Users/BerjD/Desktop/Capstone/Capstone-DataScience/en_US.news.txt", col_names = FALSE)
data_blogs <-read_tsv("C:/Users/BerjD/Desktop/Capstone/Capstone-DataScience/en_US.blogs.txt", col_names = FALSE)
summary(data_X)


data_X <- data_X %>%
  mutate(length = nchar(as.character(X1)))

data_news <- data_news %>%
  mutate(length = nchar(as.character(X1)))

data_blogs <- data_blogs %>%
  mutate(length = nchar(as.character(X1)))

data_X$length[which.max(data_X$length)]
data_news$length[which.max(data_news$length)]
data_blogs$length[which.max(data_blogs$length)]



# number of lines that have love in lower case
data_X %>%
  filter(str_detect(X1, "\\blove\\b")) %>%
  nrow()
#67243


# number of lines that have hate in lower case
data_X %>%
  filter(str_detect(X1, "\\bhate\\b")) %>%
  nrow()
#14011

67243/14011

data_X %>%
  filter(str_detect(X1, "\\bbiostats\\b"))


#number of lines that have "A computer once beat me at chess, but it was no match for me at kickboxing"
data_X %>%
  filter(str_detect(X1, "\\bA computer once beat me at chess, but it was no match for me at kickboxing\\b")) %>%
  nrow()
