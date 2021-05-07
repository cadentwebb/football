
library(nflfastR)
library(tidyverse)
library(vroom)

# Get all pass and rush plays from first 6 weeks of 2017 season
databowlPlays2017 <- vroom("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv")

# Get all play by play data from 2017 season
pbpPlays2017 <- load_pbp(2017)


# Change column names in pbp to make it easily joinable to allPlays2017
colnames(pbpPlays2017)[3] <- "gameId"
colnames(pbpPlays2017)[1] <- "playId"
#Change gameId in pbp data to a double
pbpPlays2017$gameId <- as.double(pbpPlays2017$gameId)

# Join the datasets together
plays2017 <- inner_join(databowlPlays2017, pbpPlays2017, 
                        by = c("gameId", "playId"))

# Decide which variables to keep. This gives us...

### Variables
# defenders in the box
# number of pass rushers
# personnel of defense
# play type

### Other information
# play description
# game ID
# play ID

plays2017 <- plays2017 %>% 
  select(gameId, playId, playDescription, 
         defendersInTheBox, numberOfPassRushers, 
         personnel.defense, play_type,
         passer_player_name) %>% 
  # Eliminate special teams plaeys - keep only runs and passes
  filter(play_type %in% c("run", "pass"))

# Show how many plays each QB is the thrower in
plays2017 %>% 
  group_by(passer_player_name) %>% 
  filter(!is.na(passer_player_name)) %>% 
  summarize(n())




# Example of adding the QBR for Carson Palmer to plays2017
# First, get the data from Nate's file
qbr2017 <- vroom("https://raw.githubusercontent.com/cadentwebb/football/main/ESPN%20QBR%20(2017-2018)/2017-2018%20Weekly%20ESPN%20QBR.csv?token=ATH3CLAVYCCXCO6FKQMQD2LASVQJ6")

# Create a qbr column initiated with all zeros
plays2017$qbr <- rep(0, dim(plays2017)[1])

# Search each play description for the string "C.Palmer" and return TRUE if
# it's found
# Then, add in Carson Palmer's week 1 rating of 35.6 to each of those columns
#qb <- "C.Palmer"
#for (i in 1:dim(plays2017)[1]) {
#  if (grepl(qb, plays2017[[3]][i])) {
#    plays2017$qbr[i] <- qbr2017[qbr2017$short_name == "C. Palmer" & qbr2017$week_text == "Week 1", 
#        11][[1]][1]
#  }
#}




