# This is the primary script for pulling data from the web and processing it into a usable format. 
# Any scripts or functions that are written to scrape data from the web will be called and executed here.
# Because RuPaul's Drag Race is an ongoing show with new season still airing an important component of this script will be the
# 'nseason' variable. This project was begun in February, 2019, and as such will be constructed with seasons 1-10. Future
# seasons will be added programmatically as they air.

library(tidyverse)
source('src/preparation/scrape_season.R')

nseasons <- 10

# The following look imports the wikipedia data for each season up to nseasons, and saves each table as a tab-delimited file.

for(i in 1:nseasons){
  # Call scrape_season function to scrape table data from Wikipedia page for current season
  season_data <- scrape_season(i)
  
  # Create dataframe from the 1st table in season_data, which contains information about the contestants
  contestant_data <- as_tibble(season_data[[1]], .name_repair = "minimal")
  
  # Create dataframe from the 2nd table in season_data, which summarizes each participants performance in each episode
  performance_data <- as_tibble(season_data[[2]], .name_repair = "minimal")
  
  # Create dataframe from the 4th table in season_data, which contains information about performance in the mini challenges and
  # the airdates for episoes.
  challenge_data <- as_tibble(season_data[[4]], .name_repair = "minimal")
  
  # Save each dataframe to the raw folder.
  contestant_data_path <- paste('data/raw/contestant_data_s', i, sep="")
  performance_data_path <- paste('data/raw/performance_data_s', i, sep="")
  challenge_data_path <- paste('data/raw/challenge_data_s', i, sep="")
  
  write_delim(contestant_data, contestant_data_path, delim="\t")
  write_delim(performance_data, performance_data_path, delim="\t")
  write_delim(challenge_data, challenge_data_path, delim="\t")
  
}

rm(challenge_data)
rm(contestant_data)
rm(performance_data)
rm(season_data)
rm(challenge_data_path)
rm(contestant_data_path)
rm(performance_data_path)
rm(nseasons)
rm(i)
