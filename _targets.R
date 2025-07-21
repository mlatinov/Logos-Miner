#### Libraries ####
library(targets)
library(tidyverse)
library(gutenbergr)
library(tidytext)
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
    command = read_rds(file = "data/all_books")
    ),
  
  # Remove Stop Words 
  tar_target(
    name = data_cleaned_list,
    command = stop_word_remove(data_collect)
  ),
  #### Text Stat ####
  #### Term Freq ####
  ## Term Freq ##
  tar_target(
    name = term_frequency_distribution,
    command = term_freq_dist(data_cleaned_list)
  ),
  ## Zipf’s Law ##
  tar_target(
    name = zipfs_law_plot,
    command = zipfs_law_plot(data_cleaned_list)
  )
  ##  TF_Idf ##
  #### Topic Modeling ####
  ## LDA ##
  #### Sentiment ####
)