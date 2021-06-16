# Match on a continuous variable
# Use the CBPS package

# read in final dataset
prop.model <- read.csv("https://raw.githubusercontent.com/cadentwebb/football/main/Create%20Dataset/FinalDataset.csv")

library(CBPS)
library(dplyr)
# Step 0: Get the subset that I'm interested in
#this.model <- prop.model %>% filter(yardline_100 > 20 & down.x == 3 & yardsToGo < 5 & yardsToGo > 2)

# Step 1: Match the data on my treatment variable Chulls
# Since it is continuous then I will use the cbps package

prop.model$score_differential

this.model <- prop.model %>% select(defendersInTheBox, chulls, avgPassRushers, QBrating, yardline_100, chulls_ratio, score_differential, yardsToGo,
                                    totalPassBlitzGame, game_seconds_remaining, down.x)


this.model <- this.model[complete.cases(this.model),]

fit.cbps <- CBPS(chulls ~ defendersInTheBox  + QBrating + 
                   as.factor(down.x)
                 + yardline_100 + chulls_ratio + score_differential + totalPassBlitzGame + game_seconds_remaining
                 , data = this.model)

summary(fit.cbps)


length(fit.cbps$weights)
nrow(this.model)

balance(fit.cbps)




# Check my balance measures

library(cobalt)
bal.tab(fit.cbps, un = TRUE)
bal.plot(fit.cbps, var.name = "totalPassBlitzGame", which = "both")
love.plot(fit.cbps)



########################
## K Means Clustering ##
########################

# Find out the optimal number of clusters
library(factoextra)
library(NbClust)
NbClust(fit.cbps$weights, method = "kmeans")
fviz_nbclust(fit.cbps$weights)


km.res <- kmeans(fit.cbps$weights, 4, nstart = 25)