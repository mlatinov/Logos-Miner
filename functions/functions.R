

#### Stop Words Clean Function ####

stop_word_remove <- function(data){
  
  # Stop words
  stop_words <- tidytext::stop_words
  
  # Get all unique titles
  all_titles <- unique(data$title)
  
  # Initialize an empty list to store cleaned data
  cleaned_list <- list()
  
  # Loop through each title using a different name
  for (current_title in all_titles) {
    cleaned_df <- data %>%
      filter(title == current_title) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word")
    
    cleaned_list[[current_title]] <- cleaned_df
  }
  
  return(cleaned_list)
}

#### Term_frequency_distribution ####
term_freq_dist <- function(data){
  
  # All books in the list 
  book_names <- names(data)
  
  # Histogram list 
  histograms <- list()
  
  # Loop over every book and make term frequency histogram
  for (book_name in book_names) {
    
    # Subset the book from the cleaned list
    book <- data[[book_name]]
    
    # Calculate word counts
    word_counts <- book %>%
      count(word, sort = TRUE) %>%
      mutate(total = sum(n))
    
    # Create histogram plot
    p <- ggplot(word_counts, aes(n / total)) +
      geom_histogram(fill = "orange", color = "black") +
      theme_minimal() +
      labs(title = paste("Term Frequency Distribution -", book_name),
           x = "Term Frequency",
           y = "Count")
    
    # Store the plot in the list, named by book
    histograms[[book_name]] <- p
  }
  
  # Return
  return(histograms)
}

#### Zipfs Law Plot Function ####

zipfs_law_plot <- function(){
  
  
  
  
  
  
}









