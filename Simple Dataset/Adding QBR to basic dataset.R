

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
# posteam
# defteam

plays2017 <- plays2017 %>% 
  select(gameId, playId, posteam, defteam, week, playDescription, 
         defendersInTheBox, numberOfPassRushers, 
         personnel.defense, play_type,
         passer_player_name) %>% 
  # Eliminate special teams plaeys - keep only runs and passes
  filter(play_type %in% c("run", "pass"))








# Adding Weeks 1-6 QBR to dataset
# First, get the data from Nate's file

qbr2017 <- vroom("https://raw.githubusercontent.com/cadentwebb/football/main/ESPN%20QBR%20(2017-2018)/2017-2018%20Weekly%20ESPN%20QBR.csv?token=ATH3CLE44LHB7SRKOON2TN3ASWBCW")
# Get only 2017 season
qbr2017 <- qbr2017 %>% 
  filter(season == 2017)

# Add a space to the QB name in the plays2017 dataset
for (i in 1:dim(plays2017)[1]) {
  if (!is.na(plays2017$passer_player_name[i])) {
    plays2017$passer_player_name[i] <- str_replace(plays2017$passer_player_name[i], "\\.", ". ")
  }
}


# Create a qbr column initiated with all zeros for pass plays and NA for run plays
plays2017$qbr <- ifelse(plays2017$play_type == "pass", 0, NA)


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



# This code will add the defensive ranking to the plays2017 dataset
# From the pro football statistics website: https://www.pro-football-reference.com/years/2017/opp.htm
# Uses the "total defense rating" category

# Get defensive ranking data from 2017
# "Total defense" table

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
