prop.model <- final.data


data <- final.data
data$pass_type <- paste0("pass ", data$pass_length)
data$pass_type <- ifelse(data$pass_type == "pass NA", NA, data$pass_type)

data$run_type <- paste0("run ",data$run_gap)
data$run_type <- ifelse(data$run_type == "run NA", NA, data$run_type)

#Get the play classification 
data$play_class <- ifelse(is.na(data$pass_type), data$run_type, data$pass_type)

#Drop other plays like special team and stuff
mn_data <- drop_na(data, play_class)
sum(is.na(mn_data$play_class))

library(nnet)
mn_data$play_class <- as.factor(mn_data$play_class)
mn_data$down.x <- as.factor(mn_data$down.x)
mn_data$quarter <- as.factor(mn_data$quarter)

table(mn_data$play_class)

#Set short middel pass as the baseline
mn_data$play_class <- relevel(mn_data$play_class, ref = "pass short")


mn_data$score_diff <- abs(mn_data$home_score - mn_data$VisitorScore)

library(dplyr)
prop.model <- mn_data %>% dplyr::select(chulls, defendersInTheBox, avg_corner_depth, avgPassRushers, qbr, defRank, play_class,
                                    pass, numDB, yardline_100, down.x, chulls_ratio,
                                    yardsToGo, quarter, game_seconds_remaining, quarter_seconds_remaining, safety_depth, off_spread, score_diff )



prop.model <- final.data %>% select(chulls, defendersInTheBox, avg_corner_depth, avgPassRushers, qbr, defRank,
                                    pass, numDB, yardline_100, down.y, chulls_ratio,
                                    yardsToGo, quarter, game_seconds_remaining, quarter_seconds_remaining, safety_depth, off_spread, score_diff )


View(prop.model)

prop.model <- prop.model[complete.cases(prop.model),]

prop.model <- prop.model %>% filter(chulls < 500)

cor.prop <- cor(prop.model)
corrplot::corrplot(cor.prop)

summary(prop.model$chulls)

table(prop.model$play_class)




# Match on a continuous variable
# Use the CBPS package
prop.model <- read.csv("https://raw.githubusercontent.com/cadentwebb/football/main/Simple%20Dataset/FinalDataset_ratings.csv?token=AQ3PMSZIWJOK6WU3RRLP7P3AXKGQM")
write.csv(prop.model, "final.dataset.csv")

library(CBPS)
library(dplyr)
# Step 0: Get the subset that I'm interested in
#this.model <- prop.model %>% filter(yardline_100 > 20 & down.x == 3 & yardsToGo < 5 & yardsToGo > 2)

# Step 1: Match the data on my treatment variable Chulls
# Since it is continuous then I will use the cbps package

prop.model$score_differential

this.model <- prop.model %>% select(defendersInTheBox, chulls, avgPassRushers, QBrating, yardline_100, chulls_ratio, score_differential, yardsToGo,
                                   totalPassBlitzGame, game_seconds_remaining, down.x, pass) %>% filter(abs(score_differential) < 10)


this.model <- this.model[complete.cases(this.model),]

fit.cbps <- CBPS(chulls ~ defendersInTheBox  + QBrating + 
                  as.factor(down.x)
                   + yardline_100  + totalPassBlitzGame + game_seconds_remaining
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

fviz_nbclust(fit.cbps$fitted.values, kmeans, method = "wss") +
  # geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(fit.cbps$fitted.values, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(fit.cbps$fitted.values, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

km.res <- kmeans(fit.cbps$fitted.values, 3, nstart = 100)

table(km.res$cluster)

cluster.df <- cbind(this.model, km.res$cluster, fit.cbps$fitted.values)
colnames(cluster.df)[15]  <- "cluster"
colnames(cluster.df)[16]  <- "propensity"
head(cluster.df)


cluster.map <- cluster.df %>% group_by(cluster, pass) %>% summarise(mean(chulls), mean(propensity), mean(defendersInTheBox), mean(QBrating), 
                                                     mean(yardsToGo), mean(avgPassRushers), mean(yardline_100), mean(score_differential), mean(down.x))

View(cluster.map)

ggplot(cluster.map, mapping = aes(x = cluster, y = `mean(chulls)`, group = pass, fill = as.factor(pass))) + 
  geom_bar(position = position_dodge(), stat = "identity")

ggplot(cluster.map, mapping = aes(x = cluster, y = `mean(chulls)`)) + 
  geom_point()

ggplot(cluster.df , mapping = aes(x = propensity, group = cluster, fill = as.factor(cluster))) + 
  geom_density()

cluster.df


# T tests
cluster.df.1 <- cluster.df %>% filter(cluster == 1)
t.test(chulls ~ pass, data = cluster.df.1)
cluster.df.2 <- cluster.df %>% filter(cluster == 2)
t.test(chulls ~ pass, data = cluster.df.2)
cluster.df.3 <- cluster.df %>% filter(cluster == 3)
t.test(chulls ~ pass, data = cluster.df.3)
cluster.df.4 <- cluster.df %>% filter(cluster == 4)
t.test(chulls ~ pass, data = cluster.df.4)
cluster.df.5 <- cluster.df %>% filter(cluster == 5)
t.test(chulls ~ pass, data = cluster.df.5)
cluster.df.6 <- cluster.df %>% filter(cluster == 6)
t.test(chulls ~ pass, data = cluster.df.6)
cluster.df.7 <- cluster.df %>% filter(cluster == 7)
t.test(chulls ~ pass, data = cluster.df.7)
cluster.df.8 <- cluster.df %>% filter(cluster == 8)
t.test(chulls ~ pass, data = cluster.df.8)
cluster.df.9 <- cluster.df %>% filter(cluster == 9)
t.test(chulls ~ pass, data = cluster.df.9)
cluster.df.10 <- cluster.df %>% filter(cluster == 10)
t.test(chulls ~ pass, data = cluster.df.10)

# Assess how well the model does
library(rsq)
rsq(cbps.glm)














###################################
# Just first quarter close games ##
###################################


this.model <- prop.model %>% select(defendersInTheBox, chulls, avgPassRushers, QBrating, yardline_100, chulls_ratio, score_differential, yardsToGo,
                                    totalPassBlitzGame, game_seconds_remaining, down.x, pass, quarter, off_spread,
                                    avg_corner_depth) %>% filter(abs(score_differential) < 10 & quarter == 1)


this.model <- this.model[complete.cases(this.model),]

fit.cbps <- CBPS(chulls ~ score_differential + defendersInTheBox  + QBrating + 
                   as.factor(down.x) + avg_corner_depth + off_spread + 
                 + yardline_100  + totalPassBlitzGame + game_seconds_remaining
                 , data = this.model, method = "exact")

summary(fit.cbps)


length(fit.cbps$fitted.values)
nrow(this.model)

balance(fit.cbps)





# Check my balance measures

library(cobalt)
bal.tab(fit.cbps, un = TRUE)
bal.plot(fit.cbps, var.name = "totalPassBlitzGame", which = "both")
love.plot(fit.cbps)

fviz_nbclust(fit.cbps$fitted.values, kmeans, method = "wss") +
  # geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(fit.cbps$fitted.values, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(fit.cbps$fitted.values, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

km.res <- kmeans(fit.cbps$fitted.values, 3, nstart = 100)

table(km.res$cluster)

cluster.df <- cbind(this.model, km.res$cluster, fit.cbps$fitted.values)
colnames(cluster.df)[16]  <- "cluster"
colnames(cluster.df)[17]  <- "propensity"
head(cluster.df)


cluster.map <- cluster.df %>% group_by(cluster, pass) %>% summarise(mean(chulls), mean(avg_corner_depth), mean(off_spread),
                                                                    mean(propensity), mean(defendersInTheBox), mean(QBrating), 
                                                                    mean(yardsToGo), mean(avgPassRushers), mean(yardline_100), mean(score_differential), mean(down.x))

View(cluster.map)

ggplot(cluster.map, mapping = aes(x = cluster, y = `mean(chulls)`, group = pass, fill = as.factor(pass))) + 
  geom_bar(position = position_dodge(), stat = "identity")

ggplot(cluster.map, mapping = aes(x = cluster, y = `mean(chulls)`)) + 
  geom_point()

ggplot(cluster.df , mapping = aes(x = propensity, group = cluster, fill = as.factor(cluster))) + 
  geom_density()




#########################################
## Just pass plays, where do they pass? #
#########################################

###################################
# Just first quarter close games ##
###################################
table(prop.model$pass_location)
table(prop.model$pass_type)
table(prop.model$play_class)
summary(prop.model$def_deep_1)

this.model <- prop.model %>% select(defendersInTheBox, chulls, avgPassRushers, QBrating, yardline_100, chulls_ratio, score_differential, yardsToGo,
                                    totalPassBlitzGame, game_seconds_remaining, down.x, pass, pass_type, quarter, pass_length, off_spread,
                                    avg_corner_depth) %>% filter(abs(score_differential) < 10 & pass == 1 & down.x == 2 & yardsToGo < 2)


this.model <- this.model[complete.cases(this.model),]

fit.cbps <- CBPS(chulls ~ score_differential + defendersInTheBox  + QBrating + avg_corner_depth + 
                   + yardline_100  + totalPassBlitzGame + game_seconds_remaining
                 , data = this.model, method = "exact")

summary(fit.cbps)


length(fit.cbps$fitted.values)
nrow(this.model)

balance(fit.cbps)





# Check my balance measures

library(cobalt)
bal.tab(fit.cbps, un = TRUE)
bal.plot(fit.cbps, var.name = "totalPassBlitzGame", which = "both")
love.plot(fit.cbps)

fviz_nbclust(fit.cbps$fitted.values, kmeans, method = "wss") +
  # geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(fit.cbps$fitted.values, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(fit.cbps$fitted.values, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

km.res <- kmeans(fit.cbps$fitted.values, 5, nstart = 100)

table(km.res$cluster)

cluster.df <- cbind(this.model, km.res$cluster, fit.cbps$fitted.values)
colnames(cluster.df)[18]  <- "cluster"
colnames(cluster.df)[19]  <- "propensity"
head(cluster.df)


cluster.map <- cluster.df %>% group_by(cluster, pass_length) %>% summarise(mean(chulls), mean(avg_corner_depth), mean(def_deep_1),
                                                                    mean(propensity), mean(defendersInTheBox), mean(QBrating), 
                                                                    mean(yardsToGo), mean(avgPassRushers), mean(yardline_100), 
                                                                    mean(score_differential), mean(down.x))

View(cluster.map)

ggplot(cluster.map, mapping = aes(x = cluster, y = `mean(chulls)`, group = pass_length, fill = as.factor(pass_length))) + 
  geom_bar(position = position_dodge(), stat = "identity")

ggplot(cluster.map, mapping = aes(x = cluster, y = `mean(chulls)`)) + 
  geom_point()

ggplot(cluster.df , mapping = aes(x = propensity, group = cluster, fill = as.factor(cluster))) + 
  geom_density()














##################
## 1st and 10's ##
##################


prop.model$score_differential

this.model <- prop.model %>% select(defendersInTheBox, chulls, avgPassRushers, QBrating, yardline_100, chulls_ratio, score_differential, yardsToGo,
                                    totalPassBlitzGame, game_seconds_remaining, down.x, pass) %>% 
                                    filter(down.x == 1 & yardsToGo == 10 & abs(score_differential) < 10)


this.model <- this.model[complete.cases(this.model),]

fit.cbps <- CBPS(chulls ~ defendersInTheBox  + QBrating
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


km.res <- kmeans(fit.cbps$weights, 6, nstart = 100)

table(km.res$cluster)

cluster.df <- cbind(this.model, km.res$cluster, fit.cbps$weights)
colnames(cluster.df)[13]  <- "cluster"
colnames(cluster.df)[14]  <- "propensity"
head(cluster.df)

cluster.df %>% group_by(cluster, pass) %>% summarise(mean(chulls), mean(propensity), mean(defendersInTheBox), mean(QBrating), 
                                                     mean(yardsToGo), mean(score_differential), mean(totalPassBlitzGame)) 


cluster.df.1 <- cluster.df %>% filter(cluster == 1)
t.test(chulls ~ pass, data = cluster.df.1)
cluster.df.2 <- cluster.df %>% filter(cluster == 2)
t.test(chulls ~ pass, data = cluster.df.2)
cluster.df.3 <- cluster.df %>% filter(cluster == 3)
t.test(chulls ~ pass, data = cluster.df.3)
cluster.df.4 <- cluster.df %>% filter(cluster == 4)
t.test(chulls ~ pass, data = cluster.df.4)
cluster.df.5 <- cluster.df %>% filter(cluster == 5)
t.test(chulls ~ pass, data = cluster.df.5)
cluster.df.6 <- cluster.df %>% filter(cluster == 6)
t.test(chulls ~ pass, data = cluster.df.6)









