League_Defense_2020 <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/DEFENSE/League Defense 2020.xlsx")
l.d <- League_Defense_2020
summary(l.d)

l.d$Nation <- as.factor(l.d$Nation)
l.d$Pos <- as.factor(l.d$Pos)
l.d$League <- as.factor(l.d$League)
summary(l.d)

#Import 2021 test data
League_Defense_2021 <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/DEFENSE/League Defense 2021.xlsx")
l.d.t <- League_Defense_2021
summary(l.d.t)

l.d.t$Nation <- as.factor(l.d.t$Nation)
l.d.t$League <- as.factor(l.d.t$League)
l.d.t$Pos <- as.factor(l.d.t$Pos)

summary(l.d.t)

#Find optimal number of trees 
gbm.model <- gbm(l.d$Salary~., data=l.d, shrinkage=0.01, distribution = 'gaussian', cv.folds=10, n.trees=3000, verbose=F)
gbm.n.trees <- gbm.perf(gbm.model, method="cv")


#fit boosting model to training set of 2020.
Boost.model <- gbm(l.d$Salary ~ ., data = l.d, distribution = "gaussian", n.trees = gbm.n.trees,interaction.depth = 3,cv.folds=10,shrinkage=0.01,verbose=F) #Choose gaussian distribution

#Make predictions using model on test data of 2021
Boost.prob <- predict(Boost.model, newdata = l.d.t, n.trees = gbm.n.trees,type = "response")
s.pred.salary <- data.frame(Boost.prob)

write_xlsx(s.pred.salary,"C:\\Users\\praty\\OneDrive\\Desktop\\ACTL\\ACTL4001\\EXCEL Files\\Defense salary pred 2021.xlsx")
