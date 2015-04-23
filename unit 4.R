
stevens <- read.csv('stevens.csv')
pairs(stevens)

set.seed(3000)
spl <- sample.split(stevens$Reverse,0.7)

stevens_train <- stevens[spl,]
stevens_test <- stevens[!spl,]
intersect(stevens_test,stevens_train)

library(rpart)
library(rpart.plot)

stevens_tree <- rpart(Reverse ~.,stevens_train[-c(1:2)],method='class',minbucket=25)
summary(stevens_tree)
prp(stevens_tree,varlen=0)

predict1 <- predict(stevens_tree,newdata=stevens_test,type='class')
table(stevens_test$Reverse,predict1)
(41+71)/(41+71+22+36)
predict1_roc <- predict(stevens_tree,newdata=stevens_test)
pred <- prediction(predict1_roc[,2],stevens_test$Reverse)
perf = performance(pred,'tpr','fpr')
plot(perf,colorize=T,print.cutoffs.at=c(seq(0.1,1,by=0.1)))
as.numeric(performance(pred, "auc")@y.values) 


stevens_tree1 <- rpart(Reverse ~.,stevens_train[-c(1:2)],method='class',minbucket=5)
prp(stevens_tree1)

stevens_tree2<- rpart(Reverse ~.,stevens_train[-c(1:2)],method='class',minbucket=100)
prp(stevens_tree2)

#random forests
library(randomForest)

set.seed(200)
forest1 <- randomForest(as.factor(Reverse) ~.,stevens_train[-c(1:2)],nodesize = 25,ntree=200)
pred_for <- predict(forest1,newdata=stevens_test)
x<- table(stevens_test$Reverse,pred_for)
sum(diag(x))/sum(x)


library(caret)
library(e1071)


#I want to do my own k-fold CV for the simple tree

library(dplyr); library(ggplot2);
library(rpart); library(caTools)

#split dataset
set.seed(3000)
spl <- sample.split(stevens$Reverse,0.7)
stevens$Reverse <- as.factor(stevens$Reverse)
stevens_train <- stevens[spl,]
stevens_test <- stevens[!spl,]
intersect(stevens_test,stevens_train)

#define K
k= 10
#pick the points for the folds
fold_size = floor(dim(stevens_train)[1]/(k))
folds = seq(0,dim(stevens_train)[1],fold_size)
#make the last point be the size of train
folds[k+1] = dim(stevens_train)[1]

#create a df to store results
cv_results <- data.frame(i=numeric(),cp=numeric(),accuracy=numeric())

#iterate through folds
for (i in 1:k) {
  #define the train and test, exclude fold from train, keep it in test
  trainx <- stevens_train[-((folds[i]+1):folds[i+1]),]
  testx <- stevens_train[((folds[i]+1):folds[i+1]),]
  # print(intersect(trainx,testx)) #used ot validate there was no overlap
  
  #iterate through cp values
  for (j in seq(.01,.5,by=.01)) {
    #note data=trainx[-c(1:2)] is simpler than writing all complex variable names in formula
    modelx <- rpart(Reverse ~.,data=trainx[-c(1:2)],method='class',cp=j)
    predx <- predict(modelx,newdata=testx,type='class')
    #this is faster way to get the accuracy, you get 1's only when prediction and actual match
    #the mean is teh same as suming them 1's and diving by total
    accuracyx = mean(predx==testx$Reverse)  
    #append to results data frame
    cv_results <- rbind(cv_results,data.frame(i=i,cp=j,accuracy=accuracyx))
  }
}

#summarise data and visualize it
summary <- tapply(cv_results$accuracy,cv_results$cp,mean)
plot(names(summary),summary,type='l',col='red',xlab='cp',ylab='Accuracy')
points(names(summary)[which.max(summary)],summary[which.max(summary)],col='blue',pch=16)


stevens_train$Reverse <- as.factor(stevens_train$Reverse)
stevens_test$Reverse <- as.factor(stevens_test$Reverse)
numflds <- trainControl(method='cv',number=10)
cpgrid = expand.grid(.cp=seq(.01,.5,.01))
train(Reverse ~.,stevens_train[-c(1:2)],method='rpart',trControl = numflds, tuneGrid = cpgrid)

stevens_tree_cv <- rpart(Reverse ~.,stevens_train[-c(1:2)],cp=0.19)
mean(predict(stevens_tree_cv,newdata=stevens_test,type='class')==stevens_test$Reverse)
prp(stevens_tree_cv)


#read claims
claims <- read.csv(unzip('ClaimsData.csv.zip',list=T)[[1]][1])
table(claims$bucket2009)/dim(claims)[1]

set.seed(88)
spl1 <- sample.split(claims$bucket2009,0.6)
claims_train <- subset(claims,spl1==T)
claims_test <- subset(claims,spl1!=T)

mean(claims_train$age)
mean(claims_train$diabetes)

#evaluate baseline, same bucket
mean(claims_test$bucket2009==claims_test$bucket2008)
table(claims_test$bucket2009,claims_test$bucket2008)
penalty <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=T,nrow=5)

sum(as.matrix(table(claims_test$bucket2009,claims_test$bucket2008)) * penalty)/nrow(claims_test)

mean(claims_test$bucket2009==1)
sum(as.matrix(table(claims_test$bucket2009,rep(1,nrow(claims_test)))) * penalty[,1])/nrow(claims_test)

cost_model1 <- rpart(bucket2009~. - reimbursement2009,data=claims_train,method='class',cp=0.00005)
mean(claims_test$bucket2009==predict(cost_model1,newdata=claims_test,type='class'))
sum(as.matrix(table(claims_test$bucket2009,predict(cost_model1,newdata=claims_test,type='class'))) * penalty)/nrow(claims_test)


cost_model1a <- rpart(bucket2009~. - reimbursement2009,data=claims_train,method='class',cp=0.00005,parms=list(loss=penalty))
mean(claims_test$bucket2009==predict(cost_model1a,newdata=claims_test,type='class'))
sum(as.matrix(table(claims_test$bucket2009,predict(cost_model1a,newdata=claims_test,type='class'))) * penalty)/nrow(claims_test)

#recitations
library(MASS)
boston <- read.csv('boston.csv')

pairs(boston)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col='blue',pch=19)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col='red',pch=19)
points(boston$LON,boston$LAT,col=boston$NOX,pch=19)

library(ggplot2)
boston %>%  ggplot(aes(x=LON,y=LAT,color=-MEDV))+geom_point()+
  stat_smooth(method='lm',se=F,col='red')


price_tree1 <- rpart(MEDV ~ LAT + LON,data=boston)
fancyRpartPlot(price_tree1)
pred <- predict(price_tree1)

points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col='red',pch=19)
points(boston$LON[pred>=21.2],boston$LAT[pred>=21.2],col='blue',pch='$')


points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col='red',pch=19)

price_tree2 <- rpart(MEDV ~ LAT + LON,data=boston,minbucket=50)
fancyRpartPlot(price_tree2)

abline(v=-71.07,col='black')
abline(h=42.21)
abline(h=42.28275)
abline(h=42.1734)
abline(h=42.168)


library(caTools)
set.seed(123)
split_boston<- sample.split(boston$MEDV,0.7)
boston_train <- subset(boston,split_boston==T)
boston_test <- subset(boston,split_boston==F)

lin_reg <- lm(MEDV ~.,boston_train[-c(1:2)])
summary(lin_reg)

SSE_lm <- sum((predict(lin_reg,newdata=boston_test)-boston_test$MEDV)^2)

reg_tree <- rpart(MEDV ~.,boston_train[-c(1:2)])
SSE_tree <- sum((predict(reg_tree,newdata=boston_test)-boston_test$MEDV)^2)
fancyRpartPlot(reg_tree)


#do CV

#myway
library(caret)
tr = trainControl(method = "cv",number=10)
cp.grid = expand.grid(.cp=(1:10)*0.001)
tree3 <- train(MEDV ~.,data=boston_train[,-c(1:2)],method='rpart',trControl = tr,tuneGrid = cp.grid)
tree3
fancyRpartPlot(tree3$finalModel)

sum((predict(tree3$finalModel,newdata=boston_test)-boston_test$MEDV)^2)
