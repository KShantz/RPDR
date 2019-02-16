# This script will import and clean the raw performance and challenge data scraped from Wikipedia. This will be saved
# to a single, cleaned dataframe summarizing performance for each contestant in each episode in long format.

# Import necessary packages
library(gtools)
library(tidyverse)
library(lubridate)

## Import and clean performance data

performance_data_files <- list.files(path = "data/raw", pattern = "performance_data_s\\d+", full.names = TRUE)

performance_data_files <- gtools::mixedsort(performance_data_files)

performance_data <- map(performance_data_files, read_tsv)

# These data frames are currently  structured so that each column contains information from a single episode. Because seasons
# have different numbers of episodes, the data frames will need to be cleaned by season before being joined into a single
# dataframe.
#
# I only want to retain colummns with information from episodes in which there was either a challenge, or in which the winner
# was announced. Because there is variation across seasons in how episodes air (e.g. reunion episode, recap), this will be
# accomplished by using a list of keywords to identify columns containing these keywords, which indicate that the episode
# meets my criteria for inclusion.

keywords <- c("WIN", "SAFE", "ELIM", "SAFE", "HIGH", "LOW", "BTM2")

# Define function to check if column contains values in keywords list
in_keywords <- function(x){
  # x should be a vector
  out = x %in% keywords
  sum(out) > 0
}

# Define function to map over all columns in a dataframe an create a logical vector indicating whether any values in that 
# column are in the keywords list. Then return the indices of the columns containing values in the keywords list.
indices_to_keep <- function(x){
  # x should be a dataframe
  out <- map_lgl(x, in_keywords)
  indices <- c(1, unname(which(out)))
}

# Define function to create a cleaned dataframe containing only the columns with values in the keywords list, and the first
# column, which contains contestant names

clean_perf_data <- function(x){
  # x should be a data frame
  keep <- indices_to_keep(x)
  data_out <- as_tibble(x[keep])
}


# map clean_perf_data over the list of performance data to create the reduced dataframes
performance_data <- map(performance_data, clean_perf_data)

# The contestant column is inconsistently named across dataframes in that some contain a footnote, and some don't.
# This will be a problem for the following function, so I'll just fix this now.
for(i in 1:length(performance_data)){
  colnames(performance_data[[i]])[1] <- 'contestant'
}


# gather the performance data together for each dataframe
performance_data <- map(performance_data, function(x){
  gather(x, key = 'episode', value = 'performance', -contestant, convert = TRUE)
})


performance_data <- map_dfr(list(performance_data), function(x){
  bind_rows(x, .id = "season")
})

# remove footnote text
performance_data$episode <- str_remove(performance_data$episode, pattern = '\\[.*\\]')

colnames(performance_data) <- map(colnames(performance_data), function(x){str_remove(x, pattern='\\[.*\\]')})


## Import and clean challenge data to extract information about air dates and mini-challenge performance

challenge_data_files <- list.files(path = "data/raw", pattern = "challenge_data_s\\d+", full.names = TRUE)

challenge_data_files <- gtools::mixedsort(challenge_data_files)

challenge_data <- map(challenge_data_files, read_tsv)


# There's a lot of messy and redundant information here. I want 3 pieces of information from this:
# 1. The episode number
# 2. Information about who won the mini-challenge
# 3. The original air date
#
# To do this, I'll be extracting the relevant data into separate vectors to clean and then recombine into a dataframe.

challenge_data <- map(challenge_data, function(x){
  as_tibble(x[c(2,4)])
})

challenge_data <- map_dfr(challenge_data, bind_rows, .id = 'season')

# Add column to use for filtering out unneeded rows
challenge_data$for_filtering <- challenge_data[2]==challenge_data[3]

episodes <- challenge_data %>%
  select(season, episode = `No. inseason`)  %>%
  filter(nchar(episode) <= 2)

air_dates <- challenge_data %>%
  filter(for_filtering == F) %>%
  select(air_date = `Original air date`)

# remove duplicated date information
air_dates$air_date <-  str_extract(air_dates$air_date, '[A-Z][a-z]+\\s\\d{1,2},\\s20.{2}')

# parse air dates as dates and format as POSIXct
air_dates$air_date <- parse_date_time(air_dates$air_date, "b!d!Y!")


challenge_outcomes <- challenge_data %>%
  filter(for_filtering == T) %>%
  select(mini_challenge = `Original air date`)

# remove extraneous text to extract the winners of each mini-challenge
challenge_outcomes$mini_challenge <- str_extract(challenge_outcomes$mini_challenge, "Mini-Challenge Winner.*(?=\n)") %>%
  str_remove("Mini.*:\\s")

challenge_data <- bind_cols(episodes, air_dates, challenge_outcomes)

rm(episodes, air_dates, challenge_outcomes)


# Add data about performance in the mini-challenges and air dates to the performance dataframe
performance_data <- left_join(performance_data,  challenge_data, by = c("season", "episode"))

# For each participant, indicate whether they won the mini-challenge in a given episode
for(i in 1:nrow(performance_data)){
  performance_data$mini_challenge[i] <- ifelse(str_detect(performance_data$mini_challenge[i], performance_data$contestant[i]), 1, 0)
}


# save performance data
write_delim(performance_data, path = "data/processed/performance_data_cleaned.txt", delim = "\t")

rm(challenge_data, performance_data, challenge_data_files, performance_data_files, i,  keywords)
