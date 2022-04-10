##---------------Predicting Rankings of Football Players by Position----------------##

setwd("~/Downloads")

## Tournament Shooting Rankings
t.shooting <- read.csv(file = "Tournament Shooting.csv", header = TRUE)
head(t.shooting)
summary(t.s)

# Take out Player, Nation, League, Year, and Tournament Ranking
t.s <- t.shooting[,-1] # Take out Player
t.s <- t.s[,-1] # Take out nation
t.s <- t.s[,-13] # Take out League
t.s <- t.s[,-13] # Take out Year
t.s <- t.s[,-13] # Take out Tournament Ranking

#Top10 = 1 if player is in team in Top 10 Tournament Ranking and = 0 if not
pred_ts <- glm(formula = t.s$Top10~., family = "binomial", data = t.s) # Estimate logistic regression
confint(pred_ts) # 95% CIs
summary(pred_ts)

tsr <- read.csv(file = "Team Shooters Ranking.csv", header = TRUE)
head(tsr)
summary(tsr)

tsr <- tsr[,-1] # Take out Player
tsr <- tsr[,-1] # Take Out Nation
tsr <- tsr[,-3] # Take out Born
tsr <- tsr[,-13] # Take out League
tsr <- tsr[,-13] # Take out Year

pred_tsr <- predict(pred_ts, tsr)
pred_tsr

probts <- (exp(pred_tsr)/(1+exp(pred_tsr)))
probts
write.csv(probts, file = "Output TS.csv")

## Tournament Midfielder Rankings
t.midfielders <- read.csv(file = "Tournament Midfielders.csv", header = TRUE)
head(t.midfielders)
summary(t.m)

t.m <- t.midfielders[,-1] #Take out Player
t.m <- t.m[,-1] # Take out nation
t.m <- t.m[,-18] #Take out League
t.m <- t.m[,-18] #Take out Year
t.m <- t.m[,-18] #Take out Tournament Ranking
t.m <- t.m[,-11] #Take out xA.
t.m <- t.m[,-13] #Take out 1/3

pred_tm <- glm(formula = t.m$Top10~., family = "binomial", data = t.m) # Estimate logistic regression
confint(pred_tm) # 95% CIs
summary(pred_tm)

tmr <- read.csv(file = "Team Midfielders Ranking.csv", header = TRUE)
head(tmr)

tmr <- tmr[,-1] #Take out Player
tmr <- tmr[,-1] #Take out nation
tmr <- tmr[,-17] #Take out League
tmr <- tmr[,-17] #Take out Year
tmr <- tmr[,-13] #Take out 1/3

pred_tmr <- predict(pred_tm, tmr)
pred_tmr

probtm <- (exp(pred_tmr)/(1+exp(pred_tmr)))
probtm
write.csv(probtm, file = "Output TM.csv")

## Tournament Goalkeeper Rankings
t.goalkeeping <- read.csv(file = "Tournament Goalkeeping.csv", header = TRUE)
head(t.goalkeeping)
summary(t.g)

t.g <- t.goalkeeping[,-1] # Take out Player
t.g <- t.g[,-1] #Take out Nation
t.g <- t.g[,-1] #Take out Squad
t.g <- t.g[,-10] #Take out League
t.g <- t.g[,-10] #Take out Year
t.g <- t.g[,-10] #Take out Tournament Rankings

pred_tg <- glm(formula = t.g$Top10~., family = "binomial", data = t.g) #Estimate logistic regression
confint(pred_tg)
summary(pred_tg)

install.packages("readxl") # Had to import via xlsx file (csv file woudn't work)
library(readxl)                
tgr <- read_excel(path = "Team Goalkeeping Ranking.xlsx",
                  sheet = "Team Goalkeeping Ranking",
                  col_names = TRUE)
tgr <- as.data.frame(tgr)
tgr
summary(tgr)

tgr <- tgr[,-1] # Take out Player
tgr <- tgr[,-1] #Take out Nation
tgr <- tgr[,-1] #Take out Squad
tgr <- tgr[,-10] #Take out League
tgr <- tgr[,-10] #Take out Year

pred_tgr <- predict(pred_tg, tgr)
pred_tgr

probtg <- (exp(pred_tgr)/(1+exp(pred_tgr)))
probtg
write.csv(probtg, file = "Output TG.csv")

## Tournament Defense Rankings
t.defense <- read_excel(path = "Tournament Defense.xlsx",
                        sheet = "Tournament Defense",
                        col_names = TRUE)
t.defense <- as.data.frame(t.defense)
head(t.defense)
summary(t.d)

t.d <- t.defense[,-1] #Take out Player
t.d <- t.d[,-1] #Take out nation
t.d <- t.d[,-22] # Take out Year
t.d <- t.d[,-22] #Take out Tournament Ranking

pred_td <- glm(formula = t.d$Top10~., family = "binomial", data = t.d) #Estimate logistics regression
confint(pred_td)
summary(pred_td)

tdr <- read_excel(path = "Tournament Defense Ranking.xlsx",
                  sheet = "Tournament Defense Ranking",
                  col_names = TRUE)
tdr <- as.data.frame(tdr)
head(tdr)
summary(tdr)

tdr <- tdr[,-1] #Take out Player
tdr <- tdr[,-1] #Take out nation
tdr <- tdr[,-2] #Take out Squad
tdr <- tdr[,-22] #Take out League
tdr <- tdr[,-22] #Take out Year

pred_tdr <- predict(pred_td, tdr)
pred_tdr

probtd <- (exp(pred_tdr)/(1+exp(pred_tdr)))
probtd
write.csv(probtd, file = "Output TD.csv")
