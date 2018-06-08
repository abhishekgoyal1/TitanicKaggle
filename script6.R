setwd("~/Documents/kaggle/titanic/close")
result1 <- read.csv("dtree.csv")
result2 <- read.csv("third.csv")
result3 <- read.csv("xgboost.csv")

result <- result1 + result2 + result3
result$PassengerId= result1$PassengerId
result$Survived2[result$Survived>=3]=1
result$Survived2[result$Survived<3]=0
result$Survived= result$Survived2
result= result[,c(-3)]
write.csv(submit, file = "ensemble.csv", row.names = FALSE)
