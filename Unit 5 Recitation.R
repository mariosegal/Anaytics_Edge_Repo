emails <- read.csv('energy_bids.csv')
prop.table(table(emails$responsive))

library(tm)

#Process the words
corpus <- Corpus(VectorSource(emails$email))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,stopwords("english"))
corpus = tm_map(corpus,stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm_small = removeSparseTerms(dtm,0.97)

labeled_terms <- as.data.frame(as.matrix(dtm_small))
labeled_terms$responsive <- emails$responsive

#do a model
library(caTools)

set.seed(144)
split <- sample.split(labeled_terms$responsive,0.7)
train <- subset(labeled_terms,split==T)
test <- subset(labeled_terms,split==F)

library(rpart)
library(randomForest)
library(rpart.plot)
library(rattle)
library(caret)

email1 <- rpart(responsive ~ ., train,method='class')
fancyRpartPlot(email1)
confusionMatrix(predict(email1,type='class'),train$responsive,positive='1')

confusionMatrix(predict(email1,test,type='class'),test$responsive,positive='1')

#we want to not miss any of the true 1s
library(ROCR)
predROCR <- prediction(predict(email1,test)[,2],test$responsive)
perf = performance(predROCR,'tpr','fpr')
plot(perf,colorize=T)

#looks like to get 70% TPR we need like 0.15

performance(predROCR,'auc')@y.values

