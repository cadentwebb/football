

# Adding QBR to the dataset

library(nflfastR)
library(tidyverse)
library(vroom)

# Get all pass and rush plays from first 6 weeks of 2017 season
databowlPlays2017 <- vroom("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv")

# Get all play by play data from 2017 season
pbpPlays2017 <- load_pbp(2017)

pbpPlays2017


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
# week

plays2017 <- plays2017 %>% 
  select(gameId, playId, week, playDescription, 
         defendersInTheBox, numberOfPassRushers, 
         personnel.defense, play_type,
         passer_player_name) %>% 
  # Eliminate special teams plaeys - keep only runs and passes
  filter(play_type %in% c("run", "pass"))








# Adding Weeks 1-6 QBR to dataset
# First, get the data from Nate's file
qbr2017 <- vroom("https://raw.githubusercontent.com/cadentwebb/football/main/ESPN%20QBR%20(2017-2018)/2017-2018%20Weekly%20ESPN%20QBR.csv?token=ATH3CLAVYCCXCO6FKQMQD2LASVQJ6")

# Get only 2017 season
qbr2017 <- qbr2017 %>% 
  filter(season == 2017)

# Add a space to the QB name in the plays2017 dataset
for (i in 1:dim(plays2017)[1]) {
  if (!is.na(plays2017$passer_player_name[i])) {
    plays2017$passer_player_name[i] <- str_replace(plays2017$passer_player_name[i], "\\.", ". ")
  }
}


# Create a qbr column initiated with all zeros
plays2017$qbr <- rep(0, dim(plays2017)[1])


# Gather the QB names from plays 2017
qbs <- plays2017 %>% 
  group_by(passer_player_name) %>% 
  filter(!is.na(passer_player_name)) %>% 
  summarize(Count = n())


# Add Weeks 1-6 QBR for each QB to plays2017
for (i in qbs[[1]]) {
  for (j in 1:6) {
    for (k in 1:dim(plays2017[plays2017$week == j & plays2017$play_type == "pass" & plays2017$passer_player_name == i, 10])[1]) {
      plays2017[plays2017$week == j & plays2017$play_type == "pass" & plays2017$passer_player_name == i, 10][[1]][k] <- qbr2017[qbr2017$short_name == i & qbr2017$week_text == str_c("Week ", j), 11][[1]][1]
    }
  }
}