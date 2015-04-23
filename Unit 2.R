
wine <- read.csv('wine.csv')
wine_test <- read.csv('wine_test.csv')

summary(wine)


model1 = lm(Price~AGST,wine)
summary(model1)

sum(model1$residuals^2)

model2 = lm(Price~AGST+HarvestRain,wine)
summary(model2)
sum(model2$residuals^2)

model3 <- lm(Price~AGST+HarvestRain+Age+WinterRain,+FrancePop,wine)
summary(model3)
sum(model3$residuals^2) #SSE

model4 <- lm(Price ~ HarvestRain + WinterRain,data=wine)
summary(model4)
sum(model4$residuals^2)


model5 <- lm(Price ~ HarvestRain + WinterRain+AGST+Age,data=wine))
summary(model5)
sum(model5$residuals^2)

cor(wine)
pairs(wine)

predict(model5,newdata=wine_test)
sum((predict(model5,newdata=wine_test)-wine_test$Price)^2)
sum((predict(model2,newdata=wine_test)-wine_test$Price)^2)

SSE= sum((wine_test$Price - predict(model5,newdata=wine_test))^2)
SST = sum((wine_test$Price - mean(wine$Price))^2)
r2 = 1- SSE/SST
r2


#moneyball

baseball <- read.csv('baseball.csv')
baseball$RD <- baseball$RS-baseball$RA
moneyball <- subset(baseball, Year < 2002)

winsmodel <- lm(W~RD,moneyball)
plot(moneyball$W,moneyball$RD)
summary(winsmodel)

predict(winsmodel,newdata=data.frame(RD=101))
pairs(RS~.,moneyball[-c(1:3,5,6,10,11,12,13,16)])
cor(moneyball[-c(1:3,5,6,10,11,12,13,16)],use='complete.obs')

runs_model1 <- lm(RS ~ OBP+SLG+BA,moneyball)
summary(runs_model1)

runs_model2 <- lm(RS ~ OBP+SLG,moneyball)
summary(runs_model2)

#model 1 is slightly better, but 2 it is simpler, so it wins


#predict runs allowed
pairs(RA~.,moneyball[-c(1:3,4,6,10,11,12,13,16)])
cor(moneyball[-c(1:3,4,6,10,11,12,13,16)],use='complete.obs')

ra_model1 <- lm(RA ~ OOBP+OSLG,moneyball)
summary(ra_model1)


predict(runs_model2,data.frame(OBP = 0.311 , SLG = 0.405))
predict(ra_model1,data.frame(OOBP =0.297 ,OSLG=0.370))


#predict players
players = read.csv('players.csv')
predict(runs_model2,players)


#A's in 2002
predict(runs_model2,data.frame(OBP = 0.339 , SLG = 0.430))


predict(ra_model1,data.frame(OOBP =0.307 ,OSLG=0.373))
predict(winsmodel,data.frame(RD=183))   #cool 100 games




#exercise
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins_2012 = c(94,88,95,88,93,94,98,97,93,94)
wins_2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(wins_2012,teamRank)
cor(wins_2013,teamRank)
