# This script will combine data in processed dataframes to build a dataframe summarizing the performance of each participant 
# across a number of metrics in their season. The output will be the dataframe used for modeling. Note that this script will
# function dynamically in that it will be updated to include data scraped from any new sources (e.g. twitter or instagram).

# Import necessary packages
library(tidyverse)

# Import necessary dataframes
performance_data <- read_delim("data/processed/performance_data_cleaned.txt", delim = "\t", col_types = "ncncTn")
contestant_data <- read_delim("data/processed/contestant_data_cleaned.txt", delim = "\t", col_types = "nccncl")

# Count the number of contestants and episodes in each season (note: this will only be episodes meeting the earlier criteria for
# inclusion, that a challenge occur in that episode). These variable will be useful later for normalizing performance 
# metrics across seasons.
performance_data %<>% group_by(season) %>%
  mutate(n_episodes = length(unique(episode)),
         n_contestants = length(unique(contestant)))

# Create a dataframe for summarizing performance of each participant in each season, and add to it a column encoding the number
# of episodes (excluding reunions, ru-caps etc) each participant competed in.
performance_metrics <- performance_data  %>% 
  group_by(season, contestant, n_episodes, n_contestants)  %>% 
  tally(!is.na(performance))

# Count number of times each participant was safe, in the bottom 2, in the bottom or won. Then spread these into separate
# columns for bottom 2, high, low, safe and win.
performance_metrics <- performance_data  %>% 
  group_by(season, contestant, performance) %>% 
  tally() %>% 
  spread(key=performance, value=n, fill = 0) %>% 
  transmute(BTM2 = BTM2 + ELIM, HIGH = HIGH + WIN, LOW = LOW + BTM2 + ELIM, SAFE = SAFE, WIN = WIN) %>% 
  left_join(performance_metrics, .,  by=c("season", "contestant"))  %>%
  arrange(as.numeric(season), contestant)

colnames(performance_metrics) <-  c("season", "contestant", "n_episodes", "n_contestants", "n_appearances", "n_lipsync", 
                                    "n_in_top", "n_in_bottom", "n_safe", "n_wins")

# Add mini challenge performance to metrics dataframe
performance_metrics <- performance_data %>%
  group_by(season, contestant) %>%
  summarize(mini_challenge_wins = sum(as.numeric(mini_challenge), na.rm=T)) %>%
  left_join(performance_metrics, ., by = c("season", "contestant"))

# There is some unclear difference in how some of the contestant names are encoded in the contestant_data df vs in the
# performance_data. This causes a problem for joining data, so the following step gets around this by just replacing the
# contestant names in one df with those from another.
performance_metrics$contestant <- contestant_data$contestant

# create the dataframe that will be used for modeling
rpdr_data <- left_join(contestant_data, performance_metrics, by=c("season", "contestant"))



# Any transformations to existing features or creation of new features will occur below:

## At this point, it would be possible to start modeling, however the differences across seasons in the number of episodes
# and number of contestants might cause problems for modeling these data, and so I'll first normalize these metrics by the
# number of episodes and contestants in a season. I'll also normalize by number of episodes each contestant appeared in.


