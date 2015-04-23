
voting <- read.csv('gerber.csv')
cor(voting)
summary(voting$voting)

sapply(names(voting[4:8]),function(x) tapply(voting$voting,voting[,x],mean))

log1 <- glm(voting~.,data=voting[,3:7],family='binomial')
summary(log1)
mean(voting$voting==as.numeric(predict(log1,type='response')>=0.3))
mean(voting$voting==as.numeric(predict(log1,type='response')>=0.5))
mean(voting$voting==as.numeric(predict(log1,type='response')>=1))
mean(voting$voting==as.numeric(predict(log1,type='response')>=0))
summary(predict(log1,type='response'))
table(predict(lm1)>=0.3)
auc=(.68*.5) + (.3*.2)+(.3*.14*.5)+(0.3*.31)+(0.3*0.23*.5)


cart_model <- rpart(voting ~ civicduty + hawthorne + self + neighbors,data=voting)
fancyRpartPlot(cart_model)

cart_model1 <- rpart(voting ~ civicduty + hawthorne + self + neighbors,data=voting,cp=0.0)
fancyRpartPlot(cart_model1)

cart_model2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex,data=voting,cp=0.0)
fancyRpartPlot(cart_model2,cex=0.7)
tapply(voting$voting[voting$control==1],voting$sex[voting$control==1],mean)

cart_model3 <- rpart(voting ~ control,data=voting,cp=0.0)
fancyRpartPlot(cart_model3,cex=0.7,digits=6)

cart_model4 <- rpart(voting ~ control + sex,data=voting,cp=0.0)
fancyRpartPlot(cart_model4,cex=0.7,digits=6)

log2 <- glm(voting ~ control + sex,data=voting,family='binomial')
summary(log2)


Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(log2, newdata=Possibilities, type="response")


log3 <- glm(voting ~ control + sex + control:sex,data=voting,family='binomial')
summary(log3)
predict(log3, newdata=Possibilities, type="response")

save.image('hw4.rdata')


#####
##Letters

letters <- read.csv('letters_ABPR.csv')
letters$isB <- as.factor(ifelse(letters$letter=='B',1,0))

library(caTools);library(rpart);library(rpart.plot);library(rattle)
set.seed(1000)
split_l <- sample.split(letters$isB,0.5)
letters_train <- subset(letters,split_l==T)
letters_test <- subset(letters,split_l!=T)

mean(letters_test$isB==rep(0,dim(letters_test)[1]))

letter_cart1 <- rpart(isB~. - letter,letters_train,method='class')
letter_cart1
fancyRpartPlot(letter_cart1)
mean(letters_test$isB==predict(letter_cart1,letters_test,type='class'))

set.seed(1000)
library(randomForest)
rf1 = randomForest(isB~. - letter,letters_train)
rf1
mean(letters_test$isB==predict(rf1,letters_test,type='class'))

letters$letter <- as.factor(letters$letter)
set.seed(2000)
split_l1 <- sample.split(letters$letter,0.5)
letters_train1 <- subset(letters,split_l1==T)
letters_test1 <- subset(letters,split_l1!=T)
table(letters_test1$letter)

mean(letters_test1$letter==rep('P',dim(letters_test1)[1]))

letter_cart2 <- rpart(letter~. -isB ,letters_train1,method='class')
letter_cart2
mean(letters_test1$letter==predict(letter_cart2,letters_test1,type='class'))

set.seeds(1000)
rf2 <- randomForest(letter~. -isB ,letters_train1)
rf2
mean(letters_test1$letter==predict(rf2,letters_test1,type='class'))

#income census
census <- read.csv('census.csv')
set.seed(2000)
split_c <- sample.split(census$over50k,0.6)
census_train <- subset(census,split_c==T)
census_test <- subset(census,split_c!=T)

census1 <- glm(over50k~.,census_train,family='binomial')
summary(census1)

predcens <- as.numeric(predict(census1,census_test,type='response')>=0.5)
table(census_test$over50k,predcens)
(1888+9051)/(662+9051+1888+1190)
library(ROCR)
predictTest = predict(census1, type="response", newdata=census_test)
ROCRpredTest = prediction(predictTest, census_test$over50k)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


census_tree1 <-rpart(over50k~.,census_train,method='class')
fancyRpartPlot(census_tree1)
predcens1 <- as.numeric(predict(census_tree1,census_test,type='class'))
table(census_test$over50k,predcens1)
(9243+1596)/(9243+1596+1482+470)
plot(performance(ROCRpredTest,'tpr','fpr'),colorize=T,print.cutoffs.at=c(seq(0.1,1,by=0.1)))

predictTest1 = predict(census_tree1, newdata=census_test)[,2]
ROCRpredTest1 = prediction(predictTest1, census_test$over50k)
auc1 = as.numeric(performance(ROCRpredTest1, "auc")@y.values)
auc1
plot(performance(ROCRpredTest1,'tpr','fpr'),colorize=T,print.cutoffs.at=c(seq(0.1,1,by=0.1)))


set.seed(1)
trainSmall = census_train[sample(nrow(census_train), 2000), ]
set.seed(1)
rf_census <- randomForest(over50k~.,trainSmall)
predcens2 <- as.numeric(predict(rf_census,census_test))
table(census_test$over50k,predcens1)
(1596+9243)/(1596+9243+1482+470)

vu = varUsed(rf_census, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rf_census$forest$xlevels[vusorted$ix]))
varImpPlot(rf_census)


cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
set.seed(2)
census_cv <- train(over50k~.,census_train,method='rpart',
                   trControl = trainControl(method = "cv",number=10),
                   tuneGrid = cartGrid)
census_cv

census_final <- rpart(over50k~.,census_train,method='class',cp=0.002)
fancyRpartPlot(census_final)
predcens_f <- as.numeric(predict(census_final,census_test,type='class'))
table(census_test$over50k,predcens_f)
sum(diag(table(census_test$over50k,predcens_f)))/sum(table(census_test$over50k,predcens_f))



####state extra
data(state)
statedata = data.frame(state.x77)

state1 <- lm(Life.Exp~.,statedata)
summary(state1)
sum((statedata$Life.Exp-state1$fitted.values)^2)

state2<- lm(Life.Exp ~ Population+ Murder+ Frost+ HS.Grad,statedata)
summary(state2)
sum((statedata$Life.Exp-state2$fitted.values)^2)


state3 <- rpart(Life.Exp~.,statedata)
state3
fancyRpartPlot(state3)
sum((statedata$Life.Exp-predict(state3))^2)

state4 <- rpart(Life.Exp~.,statedata,minbucket=5)
state4
fancyRpartPlot(state4)
sum((statedata$Life.Exp-predict(state4))^2)

state5 <- rpart(Life.Exp~Area,statedata,minbucket=1)
state5
fancyRpartPlot(state5)
sum((statedata$Life.Exp-predict(state5))^2)

#do cv
set.seed(111)
stateGrid = expand.grid( .cp = seq(0.01,0.5,0.01))
state_cv <- train(Life.Exp~.,statedata,method='rpart',
                   trControl = trainControl(method = "cv",number=10),
                   tuneGrid = stateGrid)
state_cv

statef <- rpart(Life.Exp~.,statedata,cp=0.12)
statef

fancyRpartPlot(statef)
sum((statedata$Life.Exp-predict(statef))^2)


statef <- rpart(Life.Exp~.,statedata,cp=0.12)
statef
fancyRpartPlot(statef)
sum((statedata$Life.Exp-predict(statef))^2)

set.seed(111)
stateGrid = expand.grid( .cp = seq(0.01,0.5,0.01))
state_cv1 <- train(Life.Exp~Area,statedata,method='rpart',
                  trControl = trainControl(method = "cv",number=10),
                  tuneGrid = stateGrid)
state_cv1

statef1 <- rpart(Life.Exp~Area,statedata,cp=0.02)
statef1
myfun <- function(x, labs, digits, varlen)
{
  sprintf("%0.f", x$frame$yval)
}

fancyRpartPlot(statef1)
sum((statedata$Life.Exp-predict(statef1))^2)

save.image('HW4.R')
