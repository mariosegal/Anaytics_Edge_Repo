library(caret)
rm(list=ls()[!(ls() %in% c('test_new2','train_new2'))])


ctrl2 <- trainControl(method='repeatedcv',number=1,repeats = 1,savePredictions=T,
                      classProbs=T, verboseIter = TRUE,
                      summaryFunction=twoClassSummary,
                      index=createMultiFolds(train_new2$Popular, 10,times=1))
ptm = proc.time()
ensemble1 <- train(Popular~.,train_new2,
                   method = "glmnet",family='binomial',trControl=ctrl2)

proc.time()-ptm








install.packages('doMC')
library(doMC)
registerDoMC(cores = 4)

ptm = proc.time()
ensemble1a <- train(Popular~.,train_new2,
                   method = "glmnet",family='binomial',trControl=ctrl2)

proc.time()-ptm
