

#### Stop Words Clean Function ####

# Function to clean and tokenize monograms and bigrams
stop_word_remove <- function(
    data = data_collect,
    grams = "monograms",
    extract = "everything"){
  
  # Load model for lemmatization
  ud_model <- udpipe_load_model("lines/english-ewt-ud-2.5-191206.udpipe")
  
  # Get all unique titles
  all_titles <- unique(data$title)
  
  # Initialize an empty list 
  cleaned_list <- list()
  
  # Loop over every title
  for (current_title in all_titles) {
    
    # Filter data for the current title
    book_data <- data %>%
      filter(title == current_title)
    
    if (grams == "monograms") {
      # Tokenize
      tokens <- book_data %>%
        unnest_tokens(word, text)
      
      # Lemmatize 
      # Pass all words at once with a single doc_id
      anno <- udpipe_annotate(ud_model, x = tokens$word)
      anno_df <- as.data.frame(anno)
      
      # Check the condition of extraction only the conceptual parts
      if (extract == "conceptual") {
        cleaned_df <- anno_df %>%
          filter(upos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%
          filter(!tolower(lemma) %in% stop_words$word) %>%
          filter(!is.na(lemma)) %>%
          mutate(lemma = str_remove_all(lemma, "\\b[A-Za-z]\\b")) %>%
          mutate(lemma = str_remove_all(lemma, "\\d+")) %>%   
          filter(str_length(lemma) > 1) %>%  
          select(doc_id, lemma) %>%
          rename(word = lemma)

        # Extract only peoples and places 
      } else if (extract == "people_places") {
        cleaned_df <- anno_df %>%
          filter(upos == "PROPN") %>%  # Nouns as proxy for names
          filter(!is.na(lemma)) %>%
          mutate(lemma = str_remove_all(lemma, "\\b[A-Za-z]\\b")) %>%
          mutate(lemma = str_remove_all(lemma, "\\d+")) %>%   
          filter(str_length(lemma) > 1) %>%  
          select(doc_id, lemma) %>%
          rename(word = lemma)

        # Extract Everything from the text 
      }else if (extract == "everything") {
        cleaned_df <- anno_df %>%
          filter(!tolower(lemma) %in% stop_words$word) %>%
          filter(!is.na(lemma)) %>%
          mutate(lemma = str_remove_all(lemma, "\\b[A-Za-z]\\b")) %>%
          mutate(lemma = str_remove_all(lemma, "\\d+")) %>%   
          filter(str_length(lemma) > 2) %>%  
          select(doc_id, lemma) %>%
          rename(word = lemma)
      }
      
      # Save the result
      cleaned_list[[current_title]] <- cleaned_df
      
    } else if (grams == "bigrams") {
      # Tokenize into bigrams
      bigrams <- book_data %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      
      # Lemmatize each word 
      # Combine all words for batch processing
      all_words <- c(bigrams$word1, bigrams$word2)
      anno <- udpipe_annotate(ud_model, x = all_words)
      lemmas <- as.data.frame(anno)$lemma
      
      # Split back into word1 and word2 lemmas
      n <- nrow(bigrams)
      word1_lemma <- lemmas[1:n]
      word2_lemma <- lemmas[(n+1):(2*n)]
      
      # Create bigrams from lemmas
      bigrams_clean <- bigrams %>%
        mutate(
          word1_lemma = str_remove_all(word1_lemma, "\\b[A-Za-z]\\b|\\d+"),
          word2_lemma = str_remove_all(word2_lemma, "\\b[A-Za-z]\\b|\\d+")
        ) %>%
        filter(
          !is.na(word1_lemma), !is.na(word2_lemma),
          str_length(word1_lemma) > 2,
          str_length(word2_lemma) > 2,
          !word1_lemma %in% stop_words$word,
          !word2_lemma %in% stop_words$word,
          str_detect(word1_lemma, "^[a-zA-Z]+$"),
          str_detect(word2_lemma, "^[a-zA-Z]+$")
        ) %>%
        unite(word, word1_lemma, word2_lemma, sep = " ") %>%
        select(word)
      
      # Store
      cleaned_list[[current_title]] <- bigrams_clean
    } else {
      message("Not supported")
    }
  }
  
  # Retrun the result
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
zipfs_law_plot <- function(data) {
  
  # All book names
  book_names <- names(data)
  
  # Empty list to store plots
  zipfs_plot <- list()
  
  # Loop over each book
  for (book_name in book_names) {
    
    # Subset the book
    subset_book <- data[[book_name]]
    
    # Calculate term frequency
    term_freq_zipfs <- subset_book %>%
      count(word, sort = TRUE) %>%
      mutate(
        total = sum(n),
        rank = row_number(),
        term_freq = n / total
      )
    
    # Words to label (top 5 frequent)
    label_words <- term_freq_zipfs %>% slice(1:5)
    
    # Plot
    p <- ggplot(term_freq_zipfs, aes(x = rank, y = term_freq)) +
      geom_line(color = "red") +
      scale_x_log10() +
      scale_y_log10() +
      geom_text_repel(
        data = label_words,
        aes(label = word),
        size = 3, color = "black"
      ) +
      theme_minimal() +
      labs(
        title = paste("Zipf's Law in -", book_name),
        x = "Rank of word (log scale)",
        y = "Term frequency (log scale)"
      )
    
    # Save to list
    zipfs_plot[[book_name]] <- p
  }
  
  return(zipfs_plot)
}

#### TF-Idf Function ####
tf_idfs_make <- function(data){
  
  # All books 
  all_books <- data %>% bind_rows(.id = "title")
  
  # Book Names
  book_names <- names(data)
  
  # Compute TF-IDF
  book_tf_idfs <- all_books %>%
    count(title,word,sort = TRUE) %>%
    bind_tf_idf(word,title,n)
  
  # Empty list  
  tf_idf_list <- list()
  
  # Loop over every book and generate plot 
  for (book_name in book_names ) {
    
    # Filter for a specific book arrange and plot the result
    p <- book_tf_idfs %>%
      filter(title == book_name) %>%
      arrange(desc(tf_idf)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(word,tf_idf),y = tf_idf))+
      geom_col(fill = "steelblue")+
      coord_flip()+
      labs(
        title = paste("Term Frequancy - Inverse Document Frequancy in",book_name),
        x = "Word",
        y = "TF-IDF Score"
      )+
      theme_minimal()
    
    # Save the results 
    tf_idf_list[[book_name]] <- p
  }
  
  # Return the tf_idf list 
  return(tf_idf_list)
  
}

#### Visualizing Bigrams Function ####
bigram_viz <- function(data = bigrams, filter_n = 2) {
  
  # Get the names of the books
  book_names <- names(data)
  
  # Empty list
  bigram_list <- list()
  
  # Loop over every book 
  for (books in book_names) {
    
    # Subset by the book name
    subset_book <- data[[books]]
    
    # Count bigrams and remove id
    bigram_graph_data <- subset_book %>%
      ungroup() %>% 
      separate(word, into = c("word1", "word2"), sep = " ") %>%
      count(word1, word2, sort = TRUE) %>%
      filter(n > filter_n) %>%
      select(word1,word2,n)
    
    # Create a bigrams network
    bigram_graph <- graph_from_data_frame(bigram_graph_data)
    
    # Convert to tidygraph for analysis
    bigram_graph <- as_tbl_graph(bigram_graph)
    
    # Add centrality measures 
    bigram_graph <- bigram_graph %>%
      mutate(degree = centrality_degree())
    
    # Plot the graf
    p <- ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(width = n), alpha = 0.4, color = "steelblue") +
      scale_edge_width(range = c(0.3, 2)) +
      geom_node_point(aes(size = degree), color = "steelblue") +
      scale_size(range = c(3, 10)) +
      geom_node_text(aes(label = name),
                     repel = TRUE, size = 3, family = "Arial") +
      theme_graph()
    
    # Save the result
    bigram_list[[books]] <- p
  }
  
  # Return the plot 
  return(bigram_list)
}


