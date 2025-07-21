#### Libraries ####

## Organization ##
library(targets)

## Data Cleaning ##
library(tidyverse)
library(tidytext)
library(udpipe)

## Data Collection ##
library(gutenbergr)

## Viz
library(ggrepel)
library(reshape2)
library(wordcloud2)

#### Source Functions ####
source(file = "functions/functions.R")

#### Pipeline ####
list(
  
  # Collect all the data 
  tar_target(
    name = data_collect,
    command = read_rds(file = "data/all_book")
    ),
  
  #### Clean the Data ####
  
  ### Remove Stop Words and separate the text into mono-grams ##
  
  ## Keep only Concepts ##
  tar_target(
    name = monograms_concepts,
    command = stop_word_remove(data_collect,grams ="monograms",extract = "conceptual")
  ),
  
  ## Keep only people and places ##
  tar_target(
    name = monograms_people_places,
    command = stop_word_remove(data_collect,grams ="monograms",extract = "people_places")
  ),
  
  ## Keep only everything ##
  tar_target(
    name = monograms_everything,
    command = stop_word_remove(data_collect,grams ="monograms",extract = "everything")
  ),
  
  # Remove Stop Words and separate the text into bi-grams
  tar_target(
    name = bigrams,
    command = stop_word_remove(data_collect,grams = "bigrams")
  ),
  
  #### Text Stat ####
  
  #### Term Freq ####
  ## Term Freq for  ##
  tar_target(
    name = term_frequency_distribution,
    command = term_freq_dist(monograms_everything)
  ),
  ## Zipf’s Law ##
  tar_target(
    name = zipfs_law_plot,
    command = zipfs_law_plot(monograms_everything)
  ),
  ## TF-Idf ##
  
  # TF-Ids for everything for monograms
  tar_target(
    name = tf_idf_everything,
    command = tf_idfs_make(monograms_everything)
  ),
  # TF-Ids for Concepts for monograms
  tar_target(
    name = tf_idf_conceptual,
    command = tf_idfs_make(monograms_concepts)
  ),
  # TF-Ids for People and Places for monograms
  tar_target(
    name = tf_idf_people_places,
    command = tf_idfs_make(monograms_people_places)
  ),
  # TF-Ids for everything for bigrams
  tar_target(
    name = tf_idf_bigrams,
    command = tf_idfs_make(bigrams)
  )
  
  #### Topic Modeling ####
  ## LDA ##
  #### Sentiment ####
)