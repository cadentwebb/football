# Complete Dataset up until this point


# Adding QBR to the dataset

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
# week
# posteam
# defteam

plays2017 <- plays2017 %>% 
  select(gameId, playId, 
         down.x, yardsToGo, 
         quarter,
         pass_length, pass_location, 
         passer_player_name, pass,
         run_location, 
         rusher_player_name, rush,
         first_down, yards_gained,
         posteam, defteam, 
         week, 
         playDescription, 
         defendersInTheBox, numberOfPassRushers, 
         personnel.defense,
         sack, interception,
         play_type) %>% 
  # Eliminate special teams plays - keep only runs and passes
  filter(play_type %in% c("run", "pass"))


# Adding Weeks 1-6 QBR to dataset
# First, get the data from Nate's file

qbr2017 <- read_csv("2017-2018 Weekly ESPN QBR.csv")
# Get only 2017 season
qbr2017 <- qbr2017 %>% 
  filter(season == 2017)


# Create a qbr column initiated with all zeros for pass plays and NA for run plays
plays2017$qbr <- rep(0, dim(plays2017)[1])

# Gather the QB names from plays 2017
teams <- plays2017 %>% 
  group_by(posteam) %>% 
  filter(!is.na(posteam)) %>% 
  summarize(Count = n())

# Add Weeks 1-6 QBR for each QB to every play in plays2017
for (i in teams[[1]]) {
  for (j in 1:6) {
    for (k in 1:dim(plays2017[plays2017$week == j & plays2017$posteam == i, 12])[1]) {
      plays2017[plays2017$week == j & plays2017$posteam == i, "qbr"][[1]][k] <- qbr2017[qbr2017$team_abb == i & qbr2017$week_text == str_c("Week ", j), 11][[1]][1]
    }
  }
}


# This code will add the defensive ranking to the plays2017 dataset

# Get defensive ranking from 2017

library(rvest)

defRanks <- "https://www.pro-football-reference.com/years/2017/opp.htm" %>% 
  read_html() %>% 
  html_node("table") %>% 
  html_table()
colnames(defRanks) <- c("Rank", "Team", "Games", "PF", "TotalYds", "TotalPlays", "YardsPerPlay", 
               "Takeaways", "FumblesLost", "FirstDowns", "PassComp", "PassAtt", "PassYds",
                 "PassTD", "Int", "YardsPerPass", "PassFirstDowns", "RunAtt", "RunYds", 
                 "RunTD", "YardsPerRun", "RunFirstDowns", "Penalties", "PenaltyYds", 
                 "PenaltyFirstDowns", "OffenseScorePercent", "TakeawayPercent",
                 "ExpectedPoints")
defRanks <- defRanks %>% 
  slice(2:33) 

# Get team abbreviations

teamAbbreviations <- plays2017 %>% 
  group_by(defteam) %>% 
  summarize(Count = n())

# Set up the data to match with the defensive ranking data
# Give defRanks the appropriate team abbreviations
# First, change Oakland to Las Vegas
teamAbbreviations$defteam <- ifelse(teamAbbreviations$defteam == "LV", "OAK", teamAbbreviations$defteam)

# Make teamAbbreviations an atomic vector
teamAbbreviations <- teamAbbreviations[[1]]

# Organize the names in alphabetical order
teamAbbreviations <- teamAbbreviations[order(teamAbbreviations)]

# Switch LA Rams and LA Chargers (helps with alphabetical order later on)
x <- teamAbbreviations[18] 
teamAbbreviations[18] <- teamAbbreviations[17]
teamAbbreviations[17] <- x
teamAbbreviations

# Give defRanks the appropriate team abbreviations
defRanks$TeamAbb <- rep(0, dim(defRanks)[1])
defRanks[order(defRanks[, "Team"]), ]$TeamAbb <- teamAbbreviations

# Now, apply defensive rank to the plays2017 dataset
plays2017$defRank <- rep(0, dim(plays2017)[1])
for (i in 1:dim(plays2017)[1]) {
  for (j in defRanks$TeamAbb) {
    if (plays2017$defteam[i] == j) {
      plays2017$defRank[i] <- defRanks[defRanks$TeamAbb == j, 1]
    }
  }
}

plays2017$defRank <- as.integer(plays2017$defRank)


# Adding number of sacks, interceptions and average number of pass rushers
# at every point in the game

# First, gather all gameIds
gameIds <- plays2017 %>% 
  group_by(gameId) %>% 
  summarize(Count = n())
gameIds <- gameIds[[1]]


# Loops through the dataset and inserts the total number of sacks, interceptions,
# and average pass rushers thus far in a game.
# Very inefficient - took about 2 minutes to run. Could use fixing.
plays2017$totalSacksGame <- rep(0, dim(plays2017)[1])
plays2017$totalIntGame <- rep(0, dim(plays2017)[1])
plays2017$avgPassRushers <- rep(0, dim(plays2017)[1])
for (i in gameIds) {
  for (j in teams[[1]]) {
    totalSacks <- 0
    totalInt <- 0
    avgPassRushers <- 0
    sumPassRushers <- 0
    passRushPlays <- 0
    for (k in 1:dim(plays2017)[1]) {
      if (plays2017$gameId[k] == i & plays2017$defteam[k] == j) {
        totalSacks <- totalSacks + plays2017$sack[k]
        plays2017$totalSacksGame[k] <- totalSacks
        totalInt <- totalInt + plays2017$interception[k]
        plays2017$totalIntGame[k] <- totalInt
        if (!is.na(plays2017$numberOfPassRushers[k])) {
          passRushPlays <- passRushPlays + 1
          sumPassRushers <- sumPassRushers + plays2017$numberOfPassRushers[k]
          avgPassRushers <- sumPassRushers / passRushPlays
          plays2017$avgPassRushers[k] <- avgPassRushers
        } else {
          plays2017$avgPassRushers[k] <- avgPassRushers
        }
      }
    }
  }
}



# Add number of defensive personnel on any give play
plays2017$numDL <- rep(0, dim(plays2017)[1])
plays2017$numLB <- rep(0, dim(plays2017)[1])
plays2017$numDB <- rep(0, dim(plays2017)[1])
for (i in 1:dim(plays2017)[1]) {
  plays2017$numDL[i] <- substr(plays2017$personnel.defense[i], 1, 1)
  plays2017$numLB[i] <- substr(plays2017$personnel.defense[i], 7, 7)
  plays2017$numDB[i] <- substr(plays2017$personnel.defense[i], 13, 13)
}

plays2017$numDL <- as.integer(plays2017$numDL)
plays2017$numLB <- as.integer(plays2017$numLB)
plays2017$numDB <- as.integer(plays2017$numDB)


# Add whether or not the defense blitzed on a pass play
plays2017$isBlitz <- ifelse(plays2017$numberOfPassRushers > 4, 1, 0)


write.csv(plays2017, "CompleteDataset.csv")







