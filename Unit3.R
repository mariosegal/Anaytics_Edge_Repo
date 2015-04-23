#Unit3

care <- read.csv('quality.csv')
plot(Narcotics~OfficeVisits,data=care,col=PoorCare+1,pch=16)

set.seed(88)
split=sample.split(care$PoorCare,SplitRatio=0.75)

care_train <- care[split,]
care_test <- care[!split,]

care_log1 <- glm(PoorCare~OfficeVisits + Narcotics,data=care_train,family='binomial')
summary(care_log1)

predict_train <- predict(care_log1,type='response')
tapply(predict_train,care_train$PoorCare,mean)
plot(predict_train,care_train$PoorCare,col=care_train$PoorCare+1,pch=16)


care_log2 <- glm(PoorCare~StartedOnCombination + ProviderCount,data=care_train,family='binomial')
summary(care_log2)

#quick exrcise
library(ROCR)
predictTest = predict(care_log1, type="response", newdata=care_test)
ROCRpredTest = prediction(predictTest, care_test$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


#Framingham
framingham <- read.csv('framingham.csv')
pairs(TenYearCHD~.,framingham,col='cyan')

framingham %>% gather(variable,value,-TenYearCHD) %>% 
  ggplot(aes(y=TenYearCHD,x=value,color=variable))+
  geom_jitter(position=position_jitter(height=.1,width=0))+
  stat_smooth(method='lm')+facet_wrap(~variable,scales='free_x')+
  theme(legend.position='none')

set.seed(1000)
split = sample.split(framingham$TenYearCHD,0.65)
framinghamTrain <- framingham[split,]
framinghamTest <- framingham[!split,]
intersect(row.names(framinghamTest),row.names(framinghamTrain))

chd_log1 <- glm(TenYearCHD~.,framinghamTrain,family='binomial')
summary(chd_log1)

predictTest <- predict(chd_log1,type='response',newdata=framinghamTest)
table(framinghamTest$TenYearCHD,predictTest>0.5,dnn=c('actual','predicted'))
accuracy = (1069+11)/(1069+11+187+6)
sensitivity = 11/(18/+11)
baseline= (1069+6)/(1069+11+187+6)
ROCRpred = prediction(predictTest,framinghamTest$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)  #auc of the test set
plot(performance(ROCRpred,'tpr','fpr'),colorize=T,print.cutoffs.at=c(seq(0.1,1,by=0.1)))
plot(performance(ROCRpred,'sens','spec'),colorize=T,print.cutoffs.at=c(seq(0.1,1,by=0.1)))

library(caret)
table(framinghamTest$TenYearCHD,predictTest>0.1,dnn=c('actual','predicted'))
table(framinghamTest$TenYearCHD,predictTest>0.2,dnn=c('actual','predicted'))


pairs(TenYearCHD ~ age +male + prevalentStroke+totChol+sysBP+glucose+cigsPerDay+prevalentHyp,data=framinghamTrain)


#Recitation
#Election Prediction
polls <- read.csv('PollingData.csv')
summary(polls)

polls %>% select(1:4) %>% gather(poll,value,Rasmussen:SurveyUSA) %>% 
  ggplot(aes(y=State,x=value,shape=poll,color=factor(Year)))+
  geom_point()+geom_vline(xintercept=0,color='black')+
  theme(legend.position='bottom')

table(polls$Year)

#Imputation
install.packages("mice")
library(mice)

simple <- polls[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
set.seed(144)
imputed <- complete(mice(simple))
summary(imputed)

polls_imputed <- cbind(polls[,-which(names(polls) %in% names(imputed))],imputed)

polls_imputed1 <- read.csv('PollingData_Imputed.csv')

polls_train <- subset(polls_imputed1,Year < 2012)
polls_test <- subset(polls_imputed,Year >=2012)
table(polls_train$Republican)

#a baseline would be the leader in the poll for tha state
#can use the function sign

table(polls_train$Republican,sign(polls_train$Rasmussen))

cor(polls_train[-c(1:2)])
pairs(polls_train[-c(1:2)])

election_log1 <- glm(Republican ~ PropR,polls_train,family='binomial')
summary(election_log1)

pred1 <- predict(election_log1,type='response')
table(polls_train$Republican,pred1 >= 0.5,dnn=c('Actual',"Predicted"))

election_log2 <- glm(Republican ~ SurveyUSA + DiffCount,polls_train,family='binomial')
summary(election_log2)
pred2 <- predict(election_log2,type='response')
table(polls_train$Republican,pred2 >= 0.5,dnn=c('Actual',"Predicted"))
#better model but not significant

#check on test
#baseline

table(polls_test$Republican,sign(polls_test$Rasmussen))

table(polls_test$Republican,predict(election_log2,type='response',newdata=polls_test) >=0.5)
subset(polls_test,predict(election_log2,type='response',newdata=polls_test) >=0.5 & Republican==0)
