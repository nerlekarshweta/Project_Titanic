#setwd("E:/@@LINUX@@/R Codes/Titanic Data")
#getwd()
#train <- read.csv("E:/@@LINUX@@/R Codes/Titanic Data/train.csv", stringsAsFactors=FALSE)
#View(train)
#test <- read.csv("E:/@@LINUX@@/R Codes/Titanic Data/test.csv", stringsAsFactors=FALSE)
# View(test)
str(train)
table(train$Survived)

prop.table(table(train$Survived))
#Assuming all passeneger in output are dead. Create survival column in test

test$Survived<-rep (0,418)
submit<-data.frame(PassengerID=test$PassengerId, Survived=test$Survived)

write.csv(submit, file="theyallperish.csv", row.names = FALSE)
#Gender class Model
table(train$Sex)
table(train$Age)

prop.table(table(train$Sex,train$Survived),1)
test$Survived[test$Sex=="female"]<-1
summary(train$Age)
train$child<-0
train$child[train$Age<18]<-1
aggregate(Survived~Sex +child,data=train,FUN = function(x) sum(x)/length(x))

aggregate(Survived~Sex +child,data=train,FUN = function(x) {sum(x)/length(x)})

#Fare wise Analysis
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
table(train$Fare2)

aggregate(Survived~Sex +Pclass+Fare2,data=train,FUN = function(x) {sum(x)/length(x)})




