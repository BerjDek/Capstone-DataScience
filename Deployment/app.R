library(shiny)
library(dplyr)
library(stringr)
library(quanteda)
library(tidytext)

# Load the preprocessed n-grams data
combined_ngrams <- read.csv("combined_ngrams_800.csv")

# Define the prediction function
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
  
  # Rename columns for the final output
  colnames(results) <- c("Predicted next word by rank", "Prior Occurrences", "Prior word sequence match depth")
  
  return(results)
}

# Define UI
ui <- fluidPage(
  titlePanel("Next Word Prediction"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_text", "Enter a sentence fragment:", value = "a jury to settle the"),
      numericInput("top_n", "Number of predictions to show:", value = 8, min = 1),
      textInput("acceptable_words", "Acceptable words if you wish to limit the search (comma separated):", value = "case, matter, account, incident"),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      tableOutput("predictions")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    acceptable_words <- if (input$acceptable_words == "") NULL else unlist(str_split(input$acceptable_words, ",\\s*"))
    predictions <- predict_next_words_ngram(input$input_text, combined_ngrams, input$top_n, acceptable_words)
    output$predictions <- renderTable({
      predictions
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)






