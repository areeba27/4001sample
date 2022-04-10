#BOOSTING METHOD
install.packages("gbm")
install.packages("xgboost")
install.packages("writexl")
library("writexl")
library(xgboost)
library(gbm)
library(caret)
#Import the data
## 2020 League Shooting (only FW or FW hybrid players considered = 930) 
League_Shooting <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/League Shooting.xlsx")
l.shooting <- League_Shooting
head(l.shooting)
summary(l.shooting)
l.s <- l.shooting[,-1] # Take out Player
l.s <- l.s[,-3] # Take out squad
l.s <- l.s[,-18] # Take out year
l.s$Nation <- as.factor(l.s$Nation)
l.s$Pos <- as.factor(l.s$Pos)
l.s$League <- as.factor(l.s$League)
summary(l.s)

#Import 2021 test data
League_shooting_2021 <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/League shooting 2021.xlsx")
l.shooting.test <- League_shooting_2021
head(l.shooting.test)
summary(l.shooting.test)
l.shooting.test$Nation <- as.factor(l.shooting.test$Nation)
l.shooting.test$Pos <- as.factor(l.shooting.test$Pos)
l.shooting.test$League <- as.factor(l.shooting.test$League)
summary(l.shooting.test)


#Find optimal number of trees 
gbm.model <- gbm(l.s$Salary~., data=l.s, shrinkage=0.01, distribution = 'gaussian', cv.folds=10, n.trees=3000, verbose=F)
gbm.n.trees <- gbm.perf(gbm.model, method="cv")


#fit boosting model to training set of 2020.
Boost.model <- gbm(l.s$Salary ~ ., data = l.s, distribution = "gaussian", n.trees = gbm.n.trees,interaction.depth = 3,cv.folds=10,shrinkage=0.01,verbose=F) #Choose gaussian distribution

#Make predictions using model on test data of 2021
Boost.prob <- predict(Boost.model, newdata = l.shooting.test, n.trees = gbm.n.trees,type = "response")
s.pred.salary <- data.frame(Boost.prob)

write_xlsx(s.pred.salary,"C:\\Users\\praty\\OneDrive\\Desktop\\ACTL\\ACTL4001\\EXCEL Files\\shooting salary pred 2021.xlsx")
