#Modeling
data <- read.csv("Simple Dataset/FinalDataset.csv")

data$game_play <- as.character(data$game_play)

data <- data %>%
  select(-OLrating, -QBrating, -RBrating)

#Try and merge in madden ratings
madden <- chulls_df %>%
  select(game_play, OLrating, QBrating, RBrating)

data_ratings <- left_join(data, madden)

#Get levels for measuring outcome
#May need to take out some plays (throw aways, spikes, qb kneels etc)

#Get column for pass plays

data_ratings$pass_type <- paste0("pass ", data_ratings$pass_length, " ",data_ratings$pass_location)
data_ratings$pass_type <- ifelse(data_ratings$pass_type == "pass NA NA", NA, data_ratings$pass_type)

data_ratings$run_type <- paste0("run ", data_ratings$run_location, " ",data_ratings$run_gap)
data_ratings$run_type <- ifelse(data_ratings$run_type == "run NA NA", NA, data_ratings$run_type)

#Get the play classification 
data_ratings$play_class <- ifelse(is.na(data_ratings$pass_type), data_ratings$run_type, data_ratings$pass_type)

#Drop other plays like special team and stuff
mn_data <- drop_na(data_ratings, play_class)
sum(is.na(mn_data$play_class))

library(nnet)
mn_data$play_class <- as.factor(mn_data$play_class)
mn_data$down.x <- as.factor(mn_data$down.x)
mn_data$quarter <- as.factor(mn_data$quarter)

#split into train test
library(caret)

play_index <- createDataPartition(mn_data$play_type, p = 0.7, list = FALSE)
train_data <- mn_data[play_index, ]
test_data  <- mn_data[-play_index, ]

#Set short middel pass as the baseline

train_data$play_class <- relevel(train_data$play_class, ref = "pass short middle")

test <- multinom(play_class ~ chulls + chulls_ratio + WR_space + def_deep_1 + down.x + yardsToGo + quarter, data = train_data)

#Log odds scale
#My interpretation of logisitc regression coefficients is rusty
summary(test)

#Get z score
z <- summary(test)$coefficients/summary(test)$standard.errors
z

#Get p value
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Exponentiate coefficients
exp(coef(test))

#Look at original class distributions
prop.table(table(train_data$play_class))

#Evaluate the model
# Predicting the values for train dataset
train_vars <- train_data %>%
  select(chulls,chulls_ratio,WR_space,def_deep_1,down.x,yardsToGo,quarter)

train_vars$predicted <- predict(test, newdata = train_vars, "class")
train_vars$real <- train_data$play_class

# Building classification table
ctable <- table(train_vars$real, train_vars$predicted)
ctable

#Look at proportions for preds vs original
prop.table(table(train_data$play_class))
prop.table(table(train_vars$predicted))
#Get some plots
#dwrite <- data.frame(chulls = , down.x = rep(c(1:4),100), quarter = rep(c(1:4),100))

to_pred <- test_data %>%
  select(chulls,chulls_ratio,WR_space,def_deep_1,down.x,yardsToGo,quarter)

## store the predicted probabilities for each value of ses and write
test_preds <- cbind(to_pred, predict(test, newdata = to_pred, type = "probs", se = TRUE))

probs <- melt(test_preds, id.vars = c("chulls","chulls_ratio","WR_space","def_deep_1","down.x","yardsToGo","quarter"), value.name = "probability")

#Only run plays
run_plays <- probs %>%
  filter(variable %in% c("run left end", "run left guard", "run left tackle", "run middle NA",
                          "run right end", "run right guard", "run right tackle"))

ggplot(run_plays, aes(x = chulls, y = probability, colour = down.x)) + geom_line() + facet_grid(variable ~
                                                                                        ., scales = "free")
#Only pass plays
pass_plays <- probs %>%
  filter(variable %in% c("pass short middle", "pass deep left", "pass deep middle", "pass deep right",
                         "pass short left", "pass short right"))
ggplot(pass_plays, aes(x = chulls, y = probability, colour = down.x)) + geom_line() + facet_grid(variable ~
                                                                                                  ., scales = "free")
third_downs  <- probs %>%
  filter(down.x == 3)

ggplot(third_downs, aes(x = yardsToGo, y = probability, colour = quarter)) + geom_line() + facet_grid(variable ~
                                                                                                   ., scales = "free")
