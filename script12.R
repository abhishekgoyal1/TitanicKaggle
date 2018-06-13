setwd("~/Documents/kaggle/titanic")
train <- read.csv("~/Documents/kaggle/titanic/train.csv")
test <- read.csv("~/Documents/kaggle/titanic/test.csv")

test$Survived <- 0
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle','Ms')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir','Jonkheer','Rev','Col')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Mrs'
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
combi$Fare= combi$Fare/combi$FamilySize
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
combi$Child <- 0
combi$Child[combi$Age<17] <- 1
library(caret)
combi= combi[,-c(1,4,6,9,10,11,15,16)]
combi$Pclass <- factor(combi$Pclass)
dummies_model <- dummyVars(Survived ~ ., data=combi)
trainData_mat <- predict(dummies_model, newdata = combi)
trainData <- data.frame(trainData_mat)
trainData$Survived <- factor(combi$Survived)
train <- trainData[1:891,]
test <- trainData[892:1309,]
library('e1071')
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
svm_Radial <- train(Survived ~., data = train, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneGrid= grid_radial,
                    tuneLength = 10)
test_pred <- predict(svm_Radial, newdata = test)
test2=read.csv("~/Documents/kaggle/titanic/test.csv")
submit <- data.frame(PassengerId = test2$PassengerId, Survived = test_pred)
write.csv(submit, file = "radialsvm.csv", row.names = FALSE)
