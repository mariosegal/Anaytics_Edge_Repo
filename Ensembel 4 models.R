library(caret)
library(doMC)
library(caretEnsemble)
library(randomForest)
library(kknn)
library(nnet)
library(glmnet)
library(gbm)


load("/Volumes/Untitled/image.rdata")

n=25
set.seed(1969)
ctrla<- trainControl(method = "boot", number = n, savePredictions = T,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,verboseIter =T,
                     index=createResample(train4$Popular, n))

list3a = list(glm=caretModelSpec(method='glmnet',family='binomial',tuneLength=5),
             gbm=caretModelSpec(method='gbm',distribution='bernoulli',tuneLength=5),
             nnet=caretModelSpec(method='nnet',tuneLength=5),
             svm=caretModelSpec(method='svmPoly',tuneLength=3))

registerDoMC(cores = 4)
model_list2a <- caretList(Popular~.,data=train4[,-drop2],
                         trControl=ctrla,metric='ROC',tuneList=list3a)
save(model_list2a,file='model_list2a.rdata')

ensemble2a <- caretEnsemble(model_list2a)

#I am getting an error mesage without reason, I believe
#In any case to do a stack all you do is call train with the 4
#predictions
#and I can do that manually, and I like that better anyway
#registerDoMC(cores = 1)
indiv_preds <- sapply(model_list2a,function(x) predict.train(x,type="prob")[,2])
set.seed(4567)
ctrl_ens2 = trainControl(method='boot',number=25,savePredictions = T,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,verboseIter =T)
ensemble2a <- train(indiv_preds,train4$Popular,
                    trControl = ctrl_ens2,metric='ROC')  #rf
ensemble2a

#predict on a test set with logword count that wher emissig where 0s
#were replaced by the mean of non zeros from train

preds_test <- sapply(model_list2a,function(x) predict.train(x,newdata=test_new1a,type="prob")[,2])

pred_ens2a_rf <- predict(ensemble2a,newdata=preds_test,type='prob')
write.csv(data.frame(UniqueID=test_new1a$UniqueID,
                     Probability1=pred_ens2a_rf[,2]),
          'submission_ens2a_rf.csv',row.names=F)


ensemble2a_glm <- train(indiv_preds,train4$Popular,method='glm',
                        family='binomial',
                    trControl = ctrl_ens2,metric='ROC')  #glm
pred_ens2a_glm <- predict(ensemble2a_glm,newdata=preds_test,type='prob')
write.csv(data.frame(UniqueID=test_new1a$UniqueID,
                     Probability1=pred_ens2a_glm[,2]),
          'submission_ens2a_glm.csv',row.names=F)


ensemble2a_gbm <- train(indiv_preds,train4$Popular,method='gbm',
                        distribution='bernoulli',
                        trControl = ctrl_ens2,metric='ROC')  #gbm
pred_ens2a_gbm <- predict(ensemble2a_gbm,newdata=preds_test,type='prob')
write.csv(data.frame(UniqueID=test_new1a$UniqueID,
                     Probability1=pred_ens2a_gbm[,2]),
          'submission_ens2a_gbm.csv',row.names=F)
