#BOOSTING METHOD
install.packages("gbm")
install.packages("xgboost")
install.packages("writexl")
library("writexl")
library(xgboost)
library(gbm)
library(caret)
#Import the data
## 2020 League midfielders 
League_Midfielders_2020 <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/MIDFIELDERS/League Midfielders 2020.xlsx")
l.m <- League_Midfielders_2020
summary(l.m)

l.m$Nation <- as.factor(l.m$Nation)
l.m$Pos <- as.factor(l.m$Pos)
l.m$League <- as.factor(l.m$League)
summary(l.m)
#Import 2021 test data
League_Midfielders_2021 <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/MIDFIELDERS/League Midfielders 2021.xlsx")
l.m.t <- League_Midfielders_2021
summary(l.m.t)

l.m.t$Nation <- as.factor(l.m.t$Nation)
l.m.t$Pos <- as.factor(l.m.t$Pos)
l.m.t$League <- as.factor(l.m.t$League)
summary(l.m.t)

#Find optimal number of trees 
gbm.model <- gbm(l.m$Salary~., data=l.m, shrinkage=0.01, distribution = 'gaussian', cv.folds=10, n.trees=3000, verbose=F)
gbm.n.trees <- gbm.perf(gbm.model, method="cv")


#fit boosting model to training set of 2020.
Boost.model <- gbm(l.m$Salary ~ ., data = l.m, distribution = "gaussian", n.trees = gbm.n.trees,interaction.depth = 3,cv.folds=10,shrinkage=0.01,verbose=F) #Choose gaussian distribution

#Make predictions using model on test data of 2021
Boost.prob <- predict(Boost.model, newdata = l.m.t, n.trees = gbm.n.trees,type = "response")
s.pred.salary <- data.frame(Boost.prob)

write_xlsx(s.pred.salary,"C:\\Users\\praty\\OneDrive\\Desktop\\ACTL\\ACTL4001\\EXCEL Files\\Midfielders salary pred 2021.xlsx")
