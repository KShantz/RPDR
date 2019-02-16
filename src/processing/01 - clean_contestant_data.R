# This script will import and clean the raw contestant data scraped from Wikipedia, and save this to a cleaned dataframe.

# Import necessary packages
library(gtools)
library(tidyverse)

## Import and clean contestant data

contestant_data_files <- list.files(path = "data/raw", pattern = "contestant_data_s\\d+", full.names = TRUE)

# sort files so that the seasons are imported in the sequential order. This is necessary so that the .id argument in map_dfr
# can be used to create a column identifying each season
contestant_data_files <- gtools::mixedsort(contestant_data_files)

contestant_data <- map(contestant_data_files, read_tsv)

contestant_data <- map_dfr(list(contestant_data), function(x){
  bind_rows(x, .id = "season")
})


# rename outcome column and modify it to encode whether a contestant won their season.
colnames(contestant_data)[6] <- 'was_winner'

contestant_data$was_winner <- as.factor(ifelse(contestant_data$was_winner == "Winner", 1, 0))


# Convert all column names to lowercase and remove text that reflects footnotes in the original Wikipedia articles
colnames(contestant_data) <- map(colnames(contestant_data), tolower)

contestant_data <- map_df(contestant_data, function(x){
  str_remove(x, "\\[.*\\]")
})

# Arrange the by season and contestant to make it easier to examine. This will also help deal with an issue in the string
# encoding in the contestant column which creates a problem for joining dataframes by contestant.
contestant_data <- contestant_data %>%
  arrange(as.numeric(season), contestant)

# save contestant_data
write_delim(contestant_data, path = "data/processed/contestant_data_cleaned.txt", delim="\t")

rm(contestant_data, contestant_data_files)