

#### Libraries ####
library(tidyverse)
library(gutenbergr)

#### Load data #### 

#### Load Nitzche Corpus ####
nietzsche_id <- gutenberg_authors %>%
  filter(grepl("Nietzsche", author)) %>%
  pull(gutenberg_author_id)

# Now use the numeric author ID to filter metadata
nietzsche_books <- gutenberg_metadata %>%
  filter(gutenberg_author_id == nietzsche_id, has_text == TRUE & language == "en")

# Books ids 
nietzsche_books_ids <- c(1998,4363,38145,37841,51356,52914,52915,52190)

## Download 
# [1] "Thus Spake Zarathustra: A Book for All and None"                              
# [2] "Beyond Good and Evil"                                                         
# [3] "Human, All Too Human: A Book for Free Spirits"                                
# [4] "Human, All-Too-Human: A Book for Free Spirits, Part 2"                        
# [5] "The Birth of Tragedy; or, Hellenism and Pessimism"                            
# [6] "The Will to Power: An Attempted Transvaluation of All Values. Book I and II"  
# [7] "The Will to Power: An Attempted Transvaluation of All Values. Book III and IV"
# [8] "Ecce Homo\nComplete Works, Volume Seventeen"  

nietzsche_corpus <- gutenberg_download(nietzsche_books_ids, meta_fields = "title")

#### Load Dostoevsky Corpus #### 

# Search for Dostoevsky
dostoevsky_id <- gutenberg_authors %>%
  filter(str_detect("Dostoyevsky, Fyodor ", author)) %>%
  pull(gutenberg_author_id)

# Now use the numeric author ID to filter metadata
dostoevsky_books <- gutenberg_metadata %>%
  filter(gutenberg_author_id == dostoevsky_id, has_text == TRUE & language == "en")

# Books ids 
dostoevsky_books_ids <- c(600,2197,2554,2638,28054)

# Download Dostoevsky Corpus 
# 1 "Notes from the Underground" 
# 2 "The Gambler"                
# 3 "Crime and Punishment"      
# 4 "The Idiot"
# 5 "The Brothers Karamazov" 

dostoevsky_corpus <- gutenberg_download(dostoevsky_books_ids, meta_fields = "title")

#### Load Kafka Corpus #### 

# Search for Franz Kafka
kafka_id <- gutenberg_authors %>%
  filter(str_detect(author, regex("Kafka", ignore_case = TRUE))) %>%
  pull(gutenberg_author_id)

# Now use the numeric author ID to filter metadata
kafka_books <- gutenberg_metadata %>%
  filter(gutenberg_author_id == kafka_id, has_text == TRUE & language == "en")

# Books ids
kafka_books_ids <- c(7849,5200)

# Download Kafka Corpus 
# 1  The Trial    
# 2  Metamorphosis 

kafka_corpus <- gutenberg_download(kafka_books_ids, meta_fields = "title")

#### Load  Aristotle Corpus ####

# Find author ID for Aristotle
aristotle_id <- gutenberg_authors %>%
  filter(str_detect(author, "Aristotle")) %>%
  pull(gutenberg_author_id)

# Aristotle Books 
aristotle_books <- gutenberg_metadata %>%
  filter(gutenberg_author_id == aristotle_id, has_text == TRUE & language == "en")

# Aristotle Books ids
aristotle_books_ids <- c(1974,6762,8438)

# [1] "The Poetics of Aristotle"           
# [2]"Politics: A Treatise on Government"
# [3] "The Ethics of Aristotle" 

# Aristotle Corpus
aristotle_corpus <- gutenberg_download(aristotle_books_ids, meta_fields = "title")

#### Load  Machiavelli Corpus ####

# Find author ID for Machiavelli
machiavelli_id <- gutenberg_authors %>%
  filter(str_detect(author, "Machiavelli")) %>%
  pull(gutenberg_author_id)

# Machiavelli Books 
machiavelli_books <- gutenberg_metadata %>%
  filter(gutenberg_author_id == machiavelli_id, has_text == TRUE & language == "en")

# Machiavelli Books ids
machiavelli_books_ids <- c(1232,2464,10827)

# [1] "The Prince"                                                                                                        
# [2] "History of Florence and of the Affairs of Italy\r\nFrom the Earliest Times to the Death of Lorenzo the Magnificent"
# [3] "Discourses on the First Decade of Titus Livius"

# Machiavelli Corpus
machiavelli_corpus <- gutenberg_download(machiavelli_books_ids, meta_fields = "title")

#### Load  Arthur Schopenhauer Corpus ####

# Find author ID for Schopenhauer
schopenhauer_id <- gutenberg_authors %>%
  filter(str_detect(author, "Schopenhauer, Arthur")) %>%
  pull(gutenberg_author_id)

# Schopenhauer Books 
schopenhauer_books <- gutenberg_metadata %>%
  filter(gutenberg_author_id == schopenhauer_id, has_text == TRUE & language == "en")

# Schopenhauer Books ids
schopenhauer_books_ids <- c(26586,38427,40097,40868,44929)

# [1] "The World as Will and Idea (Vol. 1 of 3)" 
# [2] "The World as Will and Idea (Vol. 2 of 3)"
# [3] "The World as Will and Idea (Vol. 3 of 3)" 
# [4] "The Basis of Morality"

# Schopenhauer Corpus
schopenhauer_corpus <- gutenberg_download(schopenhauer_books_ids, meta_fields = "title")


#### Save the data ####
write_rds(x = schopenhauer_corpus,file = "data/schopenhauer_corpus")
write_rds(x = machiavelli_corpus,file = "data/machiavelli_corpus")
write_rds(x = aristotle_corpus,file = "data/aristotle_corpus")
write_rds(x = kafka_corpus,file = "data/kafka_corpus")
write_rds(x = dostoevsky_corpus,file = "data/dostoevsky_corpus")
write_rds(x = nietzsche_corpus,file = "data/nietzsche_corpus")


