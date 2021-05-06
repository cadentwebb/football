# football
Football Analytics Project


library(nflfastR)
library(tidyverse)

# This is a rough, simple example of binding these two datasets

# pbp gathers data from the 2018 season using the nflfastR package
pbp <- load_pbp(2018)

# This just takes pbp (which has 370 variables) and chooses a select few
# to make it easier to model the join later on
pbptest <- pbp %>% 
  select(play_id, old_game_id, play_type, home_team, away_team, defteam)

# plays is from the Big Data Bowl pass data
plays <- read_csv("plays.csv")

# once gain, takes plays and makes it easier to manage. 
# I also made sure the names of the game id and play id match the names in the
# pbp data
playstest <- plays %>% 
  select(gameId, playId, playType, defendersInTheBox, numberOfPassRushers)
names(playstest) <- c("old_game_id", "play_id", "playType",
                      "defendersInTheBox", "numberOfPassRushers")
# made the game id a character so it would match the pbp data
playstest$old_game_id <- as.character(playstest$old_game_id)

# joined these two test data sets
joined <- inner_join(pbptest, playstest, by = c("play_id", "old_game_id"))
