# Reading data
setwd("~/Documents/kaggle/titanic")
train <- read.csv("~/Documents/kaggle/titanic/train.csv")
test <- read.csv("~/Documents/kaggle/titanic/test.csv")


#Filling missing age data
train$Age[train$Pclass==1 & train$Sex== 'male' & train$Survived==1 & is.na(train$Age)] = mean(train$Age[train$Pclass==1 & train$Sex== 'male' & train$Survived==1],na.rm=TRUE)
train$Age[train$Pclass==1 & train$Sex== 'male' & train$Survived==0 & is.na(train$Age)] = mean(train$Age[train$Pclass==1 & train$Sex== 'male' & train$Survived==0],na.rm=TRUE)
train$Age[train$Pclass==2 & train$Sex== 'male' & train$Survived==1 & is.na(train$Age)] = mean(train$Age[train$Pclass==2 & train$Sex== 'male' & train$Survived==1],na.rm=TRUE)
train$Age[train$Pclass==2 & train$Sex== 'male' & train$Survived==0 & is.na(train$Age)] = mean(train$Age[train$Pclass==2 & train$Sex== 'male' & train$Survived==0],na.rm=TRUE)
train$Age[train$Pclass==3 & train$Sex== 'male' & train$Survived==1 & is.na(train$Age)] = mean(train$Age[train$Pclass==3 & train$Sex== 'male' & train$Survived==1],na.rm=TRUE)
train$Age[train$Pclass==3 & train$Sex== 'male' & train$Survived==0 & is.na(train$Age)] = mean(train$Age[train$Pclass==3 & train$Sex== 'male' & train$Survived==0],na.rm=TRUE)

train$Age[train$Pclass==1 & train$Sex== 'female' & train$Survived==1 & is.na(train$Age)] = mean(train$Age[train$Pclass==1 & train$Sex== 'female' & train$Survived==1],na.rm=TRUE)
train$Age[train$Pclass==1 & train$Sex== 'female' & train$Survived==0 & is.na(train$Age)] = mean(train$Age[train$Pclass==1 & train$Sex== 'female' & train$Survived==0],na.rm=TRUE)
train$Age[train$Pclass==2 & train$Sex== 'female' & train$Survived==1 & is.na(train$Age)] = mean(train$Age[train$Pclass==2 & train$Sex== 'female' & train$Survived==1],na.rm=TRUE)
train$Age[train$Pclass==2 & train$Sex== 'female' & train$Survived==0 & is.na(train$Age)] = mean(train$Age[train$Pclass==2 & train$Sex== 'female' & train$Survived==0],na.rm=TRUE)
train$Age[train$Pclass==3 & train$Sex== 'female' & train$Survived==1 & is.na(train$Age)] = mean(train$Age[train$Pclass==3 & train$Sex== 'female' & train$Survived==1],na.rm=TRUE)
train$Age[train$Pclass==3 & train$Sex== 'female' & train$Survived==0 & is.na(train$Age)] = mean(train$Age[train$Pclass==3 & train$Sex== 'female' & train$Survived==0],na.rm=TRUE)


# Adding Child column
train$Child = 0
train$Child[train$Age<18]=1

# Actual output data
test$Survived=0
test$Survived[test$Sex== 'female']=1
test$Survived[test$Sex== 'female' & test$Fare>20 & test$Pclass==3]=0
test$Survived[test$Sex== 'male' & test$Age<18 & test$Pclass!=3]=1
# Writing output to file
submit = data.frame(PassengerId = test$PassengerId,Survived = test$Survived)
write.csv(submit, file= "third.csv", row.names = FALSE)
