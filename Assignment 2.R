
climate <- read.csv('climate_change.csv')
ggplot(climate,aes(x=factor(paste0(Year,Month)),y=Temp,group='Identity'))+
  geom_line(color='blue')+stat_smooth(color='red')
#went up but kind of reversong later

train <- subset(climate, Year <= 2006)
test <- subset(climate, Year > 2006)

pairs(Temp ~. ,train[-c(1:2)],col='blue')


temp_model1 <- lm(Temp~.,train[-c(1:2)])
summary(temp_model1)

cor(train[-c(1:2)])

temp_model2 <- lm(Temp ~ MEI + TSI + N2O+Aerosols,train)
summary(temp_model2)

step(temp_model1)
step_model <- lm(Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,train)
summary(step_model)


predict(step_model,test)
RSS <- sum((test$Temp - predict(step_model,test))^2)
TSS <- sum((mean(train$Temp) - test$Temp)^2)
r2_test <- 1 - (RSS/TSS)


############

pisa_train <- read.csv('pisa2009train.csv')
pisa_test <- read.csv('pisa2009test.csv')

pairs(readingScore~.,pisa_train[-3])
apply(pisa_train[-c(3,24)],2,function(x) cor(x,pisa_train[24],use='complete.obs'))

tapply(pisa_train$readingScore,pisa_train$male,mean)
summary(pisa_train)
which(sapply(pisa_train,function(x) sum(is.na(x))>0))

pisa_test <- na.omit(pisa_test)
pisa_train <- na.omit(pisa_train)


str(pisa_train)

pisa_train$raceeth <- relevel(pisa_train$raceeth, "White")
pisa_test$raceeth <- relevel(pisa_test$raceeth, "White")


lmScore <- lm(readingScore ~ ., pisa_train)
summary(lmScore)
rmse <- sqrt(mean((lmScore$fitted.values - pisa_train$readingScore)^2))
rmse


predTest <- predict(lmScore,pisa_test)
range(predTest)
sse_pisa <- sum((predTest-pisa_test$readingScore)^2)
rmse_test <- sqrt(mean((predTest - pisa_test$readingScore)^2))
mean(pisa_train$readingScore)
sst_test <- (sum((pisa_test$readingScore - mean(pisa_train$readingScore))^2))
rs_score_test <- 1-  sse_pisa/sst_test


#######
###FLU

flu_train <- read.csv('FluTrain.csv')
summary(flu_train)
head(flu_train)
plot(flu_train$ILI,flu_train$Queries,col='blue')
flu_train[which.max(flu_train$ILI),]

hist(flu_train$ILI)
plot(log(flu_train$ILI),flu_train$Queries,col='blue')

flu_model1 <- lm(log(ILI)~Queries,flu_train)
summary(flu_model1)
cor(log(flu_train$ILI),flu_train$Queries)^2

flu_test <- read.csv('FluTest.csv')

PredTest1 = exp(predict(flu_model1, newdata=flu_test))
plot(PredTest1,flu_test$ILI)
abline(0,1,col='red')

rmse_test_flu <- sqrt(mean((PredTest1-flu_test$ILI)^2))

library(zoo)

ILILag2 = lag(zoo(flu_train$ILI), -2, na.pad=TRUE)
plot(ILILag2,col='blue');points(flu_train$ILI,col='red')
sum(is.na(ILILag2))
plot(log(ILILag2),log(flu_train$ILI),col='green')

flu_train1 <- flu_train
flu_train1$ILILag2 = coredata(ILILag2)

flu_model2 <- lm(log(ILI)~Queries+log(ILILag2),flu_train1[-1],na.action(na.exclude))
summary(flu_model2)

flu_test1 <- data.frame(flu_test,ILILag2=coredata(lag(zoo(flu_test$ILI), -2, na.pad=TRUE)))

flu_test1$ILILag2[1] <- flu_train1$ILI[416]
flu_test1$ILILag2[2] <- flu_train1$ILI[417]

pred_flu_test <- exp(predict(flu_model2,newdata=flu_test1))
sqrt(mean((pred_flu_test-flu_test1$ILI)^2))


###state data
data(state)

statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

plot(statedata$x,statedata$y)

tapply(statedata$HS.Grad,statedata$state.region,mean)
boxplot(Murder~state.region,statedata,col=2:5)
statedata[statedata$state.region=='Northeast',][which.max(statedata$Murder[statedata$state.region=='Northeast']),]

state_model1<- lm(Life.Exp~Population+Income+Illiteracy+ Murder+ HS.Grad+ Frost+ Area,statedata)
summary(state_model1)


plot(statedata$Income, statedata$Life.Exp)
step(state_model1)

state_best <- lm(formula = Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(state_best)

which.min(state_best$fitted.values)
statedata$state.name[which.min(statedata$Life.Exp)]


which.max(state_best$fitted.values)
statedata$state.name[which.max(statedata$Life.Exp)]

which.min(abs(state_best$residuals))
which.max(abs(state_best$residuals))


#Elantra
elantra <- read.csv('elantra.csv')
elantra_train = subset(elantra,Year <=2012)
elantra_test = subset(elantra,Year >2012)

pairs(ElantraSales~.,elantra_train[3:7])
elantra_model1 <- lm(ElantraSales~.,elantra_train[3:7])
summary(elantra_model1)

library(stringr)
elantra$period <- paste0(elantra$Year,str_pad(elantra$Month,2,pad="0"))

ggplot(elantra,aes(x=elantra$period,y=elantra$ElantraSales,group=1))+geom_line(color='blue')+
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))


elantra %>% gather(variable,value,c(ElantraSales,Queries)) %>% 
  ggplot(aes(x=period,y=value,group=variable,color=variable))+
  geom_line()+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position='none')+
  facet_grid(variable~.,scales='free')

elantra_model2 <- lm(ElantraSales~.,elantra_train[c(1,3:7)])
summary(elantra_model2)

elantra_train$Month <- as.factor(elantra_train$Month)

elantra_model3 <- lm(ElantraSales~.,elantra_train[c(1,3:7)])
summary(elantra_model3)

cor(elantra[elantra$Year<=2012,c(1,3:7)])

elantra_model4 <- lm(ElantraSales~.,elantra_train[c(1,3:4,6:7)])
summary(elantra_model4)

elantra_test$Month <- as.factor(elantra_test$Month)
elantra_pred <- predict(elantra_model4,newdata=elantra_test)
SSE_elantra = sum((elantra_pred-elantra_test$ElantraSales)^2)
TSS_elantra = sum((elantra_test$ElantraSales-mean(elantra_train$ElantraSales))^2)
1-SSE_elantra/TSS_elantra

which.max(abs(elantra_pred - elantra_test$ElantraSales))
elantra_test$pred <- elantra_pred
elantra_test$abs_error <- abs(elantra_test$ElantraSales-elantra_test$pred)
elantra_test[which.max(elantra_test$abs_error),]
