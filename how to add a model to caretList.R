library(caretEnsemble)
library('mlbench')
library('pROC')
data(Sonar)
n=5
set.seed(107)

ctrl1 = trainControl(method = "boot", number = n, savePredictions = T,
                                    summaryFunction = twoClassSummary,
                                    classProbs = TRUE,verboseIter =T,
                                    index=createResample(Sonar$Class, n))
                       
 

list1 = list(glm=caretModelSpec(method='glm',family='binomial'),
                                    rpart=caretModelSpec(method='rpart'))
                       
model_list = caretList(Class~.,data=Sonar,trControl = ctrl1, tuneList = list1,metric='ROC')

#add a model
list_extra= train(Class~.,data=Sonar,trControl = ctrl1,method='rf')
list_extra1 = caretList(Class~.,data=Sonar,trControl = ctrl1, tuneList = list(rf=caretModelSpec(method='rf')),metric='ROC')

model_big = model_list  #to preserve it
model_big[['rf']] <- list_extra
model_big

ens1 <- caretEnsemble(model_list)
ens2 <- caretEnsemble(model_big)

summary(ens1)
summary(ens2)
