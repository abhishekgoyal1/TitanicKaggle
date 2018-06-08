# Reading data
library(rpart)
setwd("~/Documents/kaggle/titanic")
train2 <- read.csv("~/Documents/kaggle/titanic/train.csv")
test <- read.csv("~/Documents/kaggle/titanic/test.csv")
test$Survived <- NA
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
train <- combi[1:891,]
test <- combi[892:1309,]
alldata <- rbind(train,test)
alldata$Cabin = as.character(alldata$Cabin)
alldata$Cabin [is.na(alldata$Cabin)] = "Miss"
alldata <- alldata[!is.na(alldata$Embarked)]
alldata$Fare <- log(alldata$Fare + 1)
alldata$Parch [alldata$Parch == 9] = 0
train <- alldata[1:891,]
train$Survived= as.factor(train$Survived)
test <- alldata[892:1309,]
test$Survived= NULL

#Logistic Regression
model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
               Embarked + Title + FamilySize + FamilyID2, family = binomial(link = 'logit'), data = train)
summary(model)
predicted <- plogis(predict(model, test))

submit <- data.frame(PassengerId = test$PassengerId, Survived = predicted)
submit$Survived[submit$Survived>0.5]=1
submit$Survived[submit$Survived<=0.5]=0
write.csv(submit, file = "logregression.csv", row.names = FALSE)
