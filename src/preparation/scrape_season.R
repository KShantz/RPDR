# Function to scrape season data from Wikipedia and output a list of tables.
# Requires 'rvest'
# Takes the season number as input
scrape_season <- function(season){
  library(rvest)
  base_url <- 'https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_'
  add_season <- paste(season, ')', sep="")
  url <- paste(base_url, add_season, sep="")
  
  # parse the html for the website
  wiki_season <- read_html(url)
  
  # Extract tables from the html document using css selectors
  wiki_season_table_nodes <- html_nodes(wiki_season, '.wikitable')
  
  # Turn the extracted html tables into a list of dataframes.
  wiki_season_tables <- html_table(wiki_season_table_nodes, fill=T)
}