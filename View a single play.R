
#This is the script to plot one single play.

# Read in packages
library(tidyverse)
library(vroom)

# The first thing you need to do is change the play id to match the play id at the end of the string below this.
# For example change 2017091400 to the ID of the play that you want.


file.tracking <- "https://raw.githubusercontent.com/Schmoegurt/Big-Data-Bowl/master/Data/tracking_gameId_2017091400.csv"
tracking.example <- vroom(file.tracking)

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays)

merged.data <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum)


# This gets me the 22 players when the ball is snapped for non-special teams plays
normal_plays <- merged.data %>% filter(isSTPlay == FALSE) %>% filter(event == "ball_snap")

# Find the football
football <- merged.data %>% filter(isSTPlay == FALSE) %>% filter(displayName == "football")

# Make frame with football x coordinate
football_x <- football[football$displayName== "football" & football$frame.id == 1, c("x", "y", "playId")]

# Rename line of scrimmate
names(football_x)[1] <- "football_scrimmage"
names(football_x)[2] <- "football_y"


# Join 'em together
normal_plays_scrimmage <- normal_plays %>% left_join(football_x)

# Find the distance between players and line of scrimmage
# Offensive players negative, defensive players positive
normal_plays_scrimmage$dist_x <- normal_plays_scrimmage$football_scrimmage - normal_plays_scrimmage$x
normal_plays_scrimmage$dist_y <- normal_plays_scrimmage$football_y - normal_plays_scrimmage$y
normal_plays_scrimmage$total_distance_from_football <- sqrt(normal_plays_scrimmage$dist_y^2 + normal_plays_scrimmage$dist_x^2)


# Create an offense/defense variable
# 1 if on offense, 0 for defense
normal_plays_scrimmage$pos_team <- ifelse(normal_plays_scrimmage$homeTeamAbbr == normal_plays_scrimmage$possessionTeam, "home", "away")
normal_plays_scrimmage$offense_team <- ifelse(normal_plays_scrimmage$pos_team == normal_plays_scrimmage$team, 1, 0)


# Who is in the box?
# If not in the box then I assume that they are in coverage.
normal_plays_scrimmage$box <- ifelse(abs(normal_plays_scrimmage$dist_x) < 5 &
                                       abs(normal_plays_scrimmage$dist_y) < 8 &
                                       normal_plays_scrimmage$offense_team == 0, 1, 0)











# This is where you choose what play you want within the game. Just change what the playID is equal to.
# ie change 3745 to 63 or whatever

example.play <- normal_plays_scrimmage %>% filter(playId == 3745) %>% filter(event == "ball_snap")
## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3



defense <- example.play %>% filter(offense_team == 0)

data <- tibble(x = defense$x, y = defense$y)

convex_hull <- data %>% slice(chull(x, y))



## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

# Now plot
ggplot() + 
  ylim(ymin, ymax) + coord_fixed() +
  labs(title = paste("Play Number:", example.play$playId[1], ", Home Team:", example.play$homeTeamAbbr[1], "-",
                     example.play$HomeScoreAfterPlay[1], 
                     "Away Team:", example.play$visitorTeamAbbr[1], "-", example.play$VisitorScoreAfterPlay[1] ), 
       subtitle = paste(example.play$down[1], example.play$yardsToGo[1], example.play$playDescription[1]),
       caption = paste("Defensive Personnel:", example.play$personnel.defense[1], "       Offensive Personnel:", example.play$personnel.offense[1])) +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  #scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  #scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  
  # Player locations
  geom_point(data = example.play, aes(x = (xmax-y), y = x, color = as.factor(offense_team)), alpha = 0.7, size = 4) +
  geom_polygon(data = convex_hull, mapping = aes(x = xmax-y, y = x),alpha = 0.25) + 
  theme(legend.position = "none")
