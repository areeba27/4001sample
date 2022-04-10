League_Goalkeeping_2020 <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/GOALKEEPING/League Goalkeeping 2020.xlsx")
l.g <- League_Goalkeeping_2020
summary(l.g)

l.g$Nation <- as.factor(l.g$Nation)
l.g$League <- as.factor(l.g$League)
summary(l.g)

#Import 2021 test data
League_Goalkeeping_2021 <- read_excel("C:/Users/praty/OneDrive/Desktop/ACTL/ACTL4001/EXCEL Files/GOALKEEPING/League Goalkeeping 2021.xlsx")
l.g.t <- League_Goalkeeping_2021
summary(l.g.t)

l.g.t$Nation <- as.factor(l.g.t$Nation)
l.g.t$League <- as.factor(l.g.t$League)
summary(l.g.t)

#Find optimal number of trees 
gbm.model <- gbm(l.g$Salary~., data=l.g, shrinkage=0.01, distribution = 'gaussian', cv.folds=10, n.trees=3000, verbose=F)
gbm.n.trees <- gbm.perf(gbm.model, method="cv")


#fit boosting model to training set of 2020.
Boost.model <- gbm(l.g$Salary ~ ., data = l.g, distribution = "gaussian", n.trees = gbm.n.trees,interaction.depth = 3,cv.folds=10,shrinkage=0.01,verbose=F) #Choose gaussian distribution

#Make predictions using model on test data of 2021
Boost.prob <- predict(Boost.model, newdata = l.g.t, n.trees = gbm.n.trees,type = "response")
s.pred.salary <- data.frame(Boost.prob)

write_xlsx(s.pred.salary,"C:\\Users\\praty\\OneDrive\\Desktop\\ACTL\\ACTL4001\\EXCEL Files\\Goalkeepers salary pred 2021.xlsx")
