# read in data 
df <- read.csv("FinalDataset_ratings copy.csv")
library(tidyverse)
library(xgboost)

#Get column for pass plays
df$pass_type <- paste0("pass ", df$pass_length, " ",df$pass_location)
df$pass_type <- ifelse(df$pass_type == "pass NA NA", NA, df$pass_type)

df$run_type <- paste0("run ", df$run_location, " ",df$run_gap)
df$run_type <- ifelse(df$run_type == "run NA NA", NA, df$run_type)

#Get the play classification 
df$play_class <- ifelse(is.na(df$pass_type), df$run_type, df$pass_type)



# variable selection
df <- df[ , c("defendersInTheBox", "numberOfPassRushers", "chulls", "down.x", "yardsToGo","half_seconds_remaining", "play_class")]
#Drop other plays like special team and stuff
mn_data <- drop_na(df, play_class)
sum(is.na(mn_data$play_class))

# Convert the Species factor to an integer class starting at 0
play_class = mn_data$play_class
play_class = as.factor(play_class)
mn_data$play_class = as.factor(mn_data$play_class)
label = as.integer(mn_data$play_class)-1
mn_data$play_class = NULL
table(label)

# Split the data for training and testing (75/25 split)
n = nrow(mn_data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(mn_data[train.index,])
train.label = label[train.index]
test.data = as.matrix(mn_data[-train.index,])
test.label = label[-train.index]

  
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)



# Define the parameters for multinomial classification
num_class = length(levels(play_class))
num_class
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)


# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(play_class)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(play_class)[test.label+1]


# Calculate the final accuracy
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))

