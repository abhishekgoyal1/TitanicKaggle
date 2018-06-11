setwd("~/Documents/kaggle/titanic")
train <- read.csv("~/Documents/kaggle/titanic/train.csv")
test <- read.csv("~/Documents/kaggle/titanic/test.csv")

test$Survived <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
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
library(rpart)
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
combi$Child <- 0
combi$Child[train$Age<17] <- 1
library(caret)
combi= combi[,-c(1,4,6,9,10,11,15,16)]
combi$Pclass <- factor(combi$Pclass)
dummies_model <- dummyVars(Survived ~ ., data=combi)
trainData_mat <- predict(dummies_model, newdata = combi)
trainData <- data.frame(trainData_mat)
trainData$Survived <- combi$Survived
train <- trainData[1:891,]
test <- trainData[892:1309,]

library(randomForest)
set.seed(315)
fit <- randomForest(as.factor(Survived) ~ .,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
Prediction <- predict(fit, test)
test2=read.csv("~/Documents/kaggle/titanic/test.csv")
submit <- data.frame(PassengerId = test2$PassengerId, Survived = Prediction)
write.csv(submit, file = "randomforest3.csv", row.names = FALSE)

