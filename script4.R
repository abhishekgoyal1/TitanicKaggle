# Reading data
library(data.table)
setwd("~/Documents/kaggle/titanic")
train2 <- read.csv("~/Documents/kaggle/titanic/train.csv")
test <- read.csv("~/Documents/kaggle/titanic/test.csv")
test$Survived <- 0
combi <- rbind(train2, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme','Mlle','Miss','Ms')]= 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[is.na(combi$Fare)]= mean(combi$Fare,na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
combi2= combi[,c(-4,-9,-11,-15,-16)]
library(caret)
dmy <- dummyVars(" ~ .", data = combi2)
trsf <- data.frame(predict(dmy, newdata = combi2))
train <- trsf[1:891,]
test <- trsf[892:1309,]
setDT(train)
setDT(test)
labels <- train$Survived
ts_label <- test$Survived
new_tr <- model.matrix(~.+0,data = train[,-c(2)]) 
new_ts <- model.matrix(~.+0,data = test[,-c(2)])
labels <- as.numeric(labels)
ts_label <- as.numeric(ts_label)
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 14, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stopping_rounds = 10, maximize = F, eval_metric = "error")
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
submit <- data.frame(PassengerId = test$PassengerId, Survived = xgbpred)
write.csv(submit, file = "xgboost.csv", row.names = FALSE)

