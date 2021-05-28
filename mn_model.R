#Modeling
data <- read.csv("Simple Dataset/FinalDataset.csv")

#Get levels for measuring outcome
#May need to take out some plays (throw aways, spikes, qb kneels etc)

#Get column for pass plays

data$pass_type <- paste0("pass ", data$pass_length, " ",data$pass_location)
data$pass_type <- ifelse(data$pass_type == "pass NA NA", NA, data$pass_type)

data$run_type <- paste0("run ", data$run_location, " ",data$run_gap)
data$run_type <- ifelse(data$run_type == "run NA NA", NA, data$run_type)

#Get the play classification 
data$play_class <- ifelse(is.na(data$pass_type), data$run_type, data$pass_type)

#Drop other plays like special team and stuff
mn_data <- drop_na(data, play_class)
sum(is.na(mn_data$play_class))

library(nnet)
mn_data$play_class <- as.factor(mn_data$play_class)
mn_data$down.x <- as.factor(mn_data$down.x)
mn_data$quarter <- as.factor(mn_data$quarter)

#Set short middel pass as the baseline
mn_data$play_class <- relevel(mn_data$play_class, ref = "pass short middle")

test <- multinom(play_class ~ chulls + chulls_ratio + WR_space + def_deep_1 + down.x + yardsToGo + quarter, data = mn_data)

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


#Get some plots
#dwrite <- data.frame(chulls = , down.x = rep(c(1:4),100), quarter = rep(c(1:4),100))

## store the predicted probabilities for each value of ses and write
#pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))