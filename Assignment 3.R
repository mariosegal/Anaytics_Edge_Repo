songs <- read.csv('songs.csv')
table(songs$year)
sum(songs$artistname=='Michael Jackson')
subset(songs, artistname=='Michael Jackson' & Top10 ==1)
table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

songs_train <- subset(songs,year <= 2009)
songs_test <- subset(songs,year > 2009)


songs_log1 <- glm(Top10 ~.,data=songs_train[-c(1:5)],family='binomial')
summary(songs_log1)
cor(songs_train[-c(1:5)])

songs_log2 <- glm(Top10 ~. - loudness,data=songs_train[-c(1:5)],family='binomial')
summary(songs_log2)

songs_log3 <- glm(Top10 ~. - energy,data=songs_train[-c(1:5)],family='binomial')
summary(songs_log3)


table(songs_test$Top10,predict(songs_log3,newdata=songs_test,type='response')>=0.45,
      dnn=c('Actual','Prediction'))
(309+19)/(309+19+40+5)

sensitivity = 19/59
specificity = 309/314

#BASELINE
table(songs_test$Top10)
314/(314+59)


##############
#PAROLE

parole <- read.csv('parole.csv')
head(parole)
pairs(parole,col='blue')
summary(parole)
table(parole$violator)

sapply(parole,table)

parole$crime <- factor(parole$crime,1:4)
parole$state <- factor(parole$state,1:4)
  

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

parole_log1 <- glm(violator~.,data=train,family='binomial')
summary(parole_log1)

-4.2411574+0.3869904+0.8867192+-0.0001756*50+-0.1238867*3+0.0802954*12+0.6837143
exp(-4.2411574+0.3869904+0.8867192+-0.0001756*50+-0.1238867*3+0.0802954*12+0.6837143)

predict(parole_log1,newdata=data.frame(male=1,race=1,age=50,state=as.factor(1),max.sentence=12,time.served=3
                                       ,multiple.offenses=0,crime=as.factor(2)),type='response')

male, white race, aged 50 years at prison release, from the state of Maryland, 
served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, 
and committed a larceny


parole_pred_test <- predict(parole_log1,newdata=test,type='response')
summary(parole_pred_test)
table(test$violator,parole_pred_test>=0.5)
(179/202)

table(test$violator)

parole_pred = prediction(parole_pred_test,test$violator)
as.numeric(performance(parole_pred, "auc")@y.values) 

plot(performance(parole_pred, "tpr",'fpr'),colorize=T)

#loans

loans <- read.csv('loans.csv')
pairs(loans)
str(loans)
ggplot(loans,aes(y=not.fully.paid,x=loans$dti))+geom_jitter(alpha=0.3,color='red')
summary(loans)
dim(na.exclude(loans))
bad <- loans[which(apply(loans,1,function(x) sum(is.na(x)))>0),]


loans1 <- read.csv('loans_imputed.csv')
set.seed(144)
split <- sample.split(loans1$not.fully.paid,0.7)
train <- loans1[split,]
test <- loans1[!split,]

credit_log1 <- glm(not.fully.paid~.,train,family='binomial')
summary(credit_log1)

700*-9.317e-03 - 710*-9.317e-03

exp(700*-9.317e-03)/exp(710*-9.317e-03) #which is the same as
exp(-10*-9.317e-03)
pred_test <- predict(credit_log1,newdata = test,type='response')
table(test$not.fully.paid,pred_test>=0.5)
2403/2873
test$predicted.risk = pred_test

table(test$not.fully.paid)
2413/2873

roc = prediction(pred_test,test$not.fully.paid)
as.numeric(performance(roc, "auc")@y.values)


credit_log2 <- glm(not.fully.paid~int.rate,train,family='binomial')
summary(credit_log2)

pred_test1 <- predict(credit_log2,newdata = test,type='response')
summary(pred_test1)
table(test$not.fully.paid,pred_test1>=0.5)

roc = prediction(pred_test1,test$not.fully.paid)
as.numeric(performance(roc, "auc")@y.values)

10 * exp(.06*3) 

test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1

targets <- subset(test,int.rate >= 0.15)
mean(targets$profit)
table(targets$not.fully.paid)

sort(targets$predicted.risk, decreasing=FALSE)[100]
targets1 <- targets[order(targets$predicted.risk),]
best <- targets1 %>% slice(1:100)
sum(best$profit)
table(best$not.fully.paid)

#extra playoffs
baseball <- read.csv('baseball.csv')
dim(unique(baseball[,c('Year','Team')]))
length(unique(baseball$Year))

playoffs <- subset(baseball,Playoffs==1)
dim(unique(playoffs[,c('Year','Team')]))
tapply(playoffs$Team,playoffs$Year,length)

playoffs <- playoffs %>% group_by(Year) %>% mutate(NumCompetitors=n_distinct(Team))

PlayoffTable = table(playoffs$Year)
names(PlayoffTable)
dim(unique(playoffs[playoffs$NumCompetitors==8,c('Year','Team')]))

playoffs$WS = ifelse(playoffs$RankPlayoffs==1,1,0)

models <- data.frame(var=character,p=numeric(),aic=numeric())
i=0
for (var in names(playoffs)[!(names(playoffs) %in% c('WS','Team','RankPlayoffs','Playoffs'))]) {
  i= i+1
  tmp <- summary(glm(WS ~.,data=playoffs[,c('WS',var)],family='binomial'))
  models = rbind(models,data.frame(p=tmp$coefficients[2,4],var=var,aic=tmp$aic))
}

models[order(models$p),]

playoff_log1 <- glm(WS~Year+RA+NumCompetitors+RankSeason,playoffs,family='binomial')
summary(playoff_log1)
cor(playoffs[,c('Year','RA',"NumCompetitors",'RankSeason')])



i=dim(models)[1]
vars1 <- c('Year','RA',"NumCompetitors",'RankSeason') 
for (a in 1:3) {
  for (b in (a+1):4 ) {
    if ( a !=b) {
      i = i+1
      tmp <- summary(glm(WS ~.,data=playoffs[,c('WS',vars1[c(a,b)])],family='binomial'))
      models = rbind(models,data.frame(p=tmp$coefficients[2,4],var=paste0(vars1[a],'/',vars1[b]),aic=tmp$aic))
    }
  }
}


models[order(models$aic),]


table(playoffs$WS,predict(glm(WS~NumCompetitors,playoffs,family='binomial'),type='response')>0.5)
