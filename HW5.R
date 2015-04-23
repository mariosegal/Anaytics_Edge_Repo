library(tm)
wiki <- read.csv('wiki.csv',stringsAsFactors=F)

wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

corpus_add <- Corpus(VectorSource(wiki$Added))
corpus_add = tm_map(corpus_add, PlainTextDocument)
corpus_add = tm_map(corpus_add,removeWords,stopwords("english"))
corpus_add = tm_map(corpus_add,stemDocument)

dtm_add = DocumentTermMatrix(corpus_add)
dtm_add

dtm_add1 <- removeSparseTerms(dtm_add,0.997)

add_terms <- as.data.frame(as.matrix(dtm_add))
names(add_terms) <- paste("A",names(add_terms),sep="_")

corpus_add <- Corpus(VectorSource(wiki$Added))
corpus_add = tm_map(corpus_add, PlainTextDocument)
corpus_add = tm_map(corpus_add,removeWords,stopwords("english"))
corpus_add = tm_map(corpus_add,stemDocument)

dtm_add = DocumentTermMatrix(corpus_add)
dtm_add

dtm_add1 <- removeSparseTerms(dtm_add,0.997)

add_terms <- as.data.frame(as.matrix(dtm_add1))
names(add_terms) <- paste("A",names(add_terms),sep="_")

#removed
corpus_remv <- Corpus(VectorSource(wiki$Removed))
corpus_remv = tm_map(corpus_remv, PlainTextDocument)
corpus_remv = tm_map(corpus_remv,removeWords,stopwords("english"))
corpus_remv = tm_map(corpus_remv,stemDocument)

dtm_remv = DocumentTermMatrix(corpus_remv)
dtm_remv

dtm_remv1 <- removeSparseTerms(dtm_remv,0.997)

remv_terms <- as.data.frame(as.matrix(dtm_remv1))
names(remv_terms) <- paste("R",names(remv_terms),sep="_")


#combine
wikiWords = cbind(remv_terms, add_terms)
wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal,0.7)
train <- subset(wikiWords,split==T)
test <- subset(wikiWords,split==F)


library(rpart)
library(caret)
library(rpart.plot)
library(rattle)
prop.table(table(test$Vandal))

wiki1 <- rpart(Vandal~.,train,method='class')
confusionMatrix(predict(wiki1,test,type='class'),test$Vandal)
fancyRpartPlot(wiki1)


wikiWords2 <- wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

train2 = subset(wikiWords2, split==TRUE)
test2 = subset(wikiWords2, split==FALSE)

wiki2 <- rpart(Vandal~.,train2,method='class')
confusionMatrix(predict(wiki2,test2,type='class'),test2$Vandal)
fancyRpartPlot(wiki2)


wikiWords2$NumWordsAdded = rowSums(as.matrix(dtm_add))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtm_remv))


train2 = subset(wikiWords2, split==TRUE)
test2 = subset(wikiWords2, split==FALSE)

wiki3 <- rpart(Vandal~.,train2,method='class')
confusionMatrix(predict(wiki3,test2,type='class'),test2$Vandal)
fancyRpartPlot(wiki3)



wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

train3 = subset(wikiWords3, split==TRUE)
test3 = subset(wikiWords3, split==FALSE)

wiki4 <- rpart(Vandal~.,train3,method='class')
confusionMatrix(predict(wiki4,test3,type='class'),test3$Vandal)
fancyRpartPlot(wiki4)


#########
trials <- read.csv('clinical_trial.csv',stringsAsFactors=F)

head(trials)
max(nchar(trials$abstract))
sum(nchar(trials$abstract)==0)
which.min(nchar(trials$title))
trials$title[1258]


corpus_title <- Corpus(VectorSource(trials$title))
corpus_title <- tm_map(corpus_title, tolower)
corpus_title = tm_map(corpus_title, PlainTextDocument)
corpus_title = tm_map(corpus_title,removePunctuation)
corpus_title = tm_map(corpus_title,removeWords,stopwords("english"))
corpus_title = tm_map(corpus_title,stemDocument)

corpus_abstract <- Corpus(VectorSource(trials$abstract))
corpus_abstract <- tm_map(corpus_abstract, tolower)
corpus_abstract = tm_map(corpus_abstract, PlainTextDocument)
corpus_abstract = tm_map(corpus_abstract, removePunctuation)
corpus_abstract = tm_map(corpus_abstract,removeWords,stopwords("english"))
corpus_abstract = tm_map(corpus_abstract,stemDocument)

dtm_title = DocumentTermMatrix(corpus_title)
dtm_abstract = DocumentTermMatrix(corpus_abstract )

dtm_title1 <- removeSparseTerms(dtm_title,0.95)
dtm_abstract1 <- removeSparseTerms(dtm_abstract,0.95)

titles <- as.data.frame(as.matrix(dtm_title1))
abstracts <- as.data.frame(as.matrix(dtm_abstract1))

which.max(colSums(abstracts))
row.names(titles) <- NULL
row.names(abstracts) <- NULL

names(titles) <- paste0('T_',names(titles))
names(abstracts) <- paste0('A_',names(abstracts))

dtm <- cbind(abstracts,titles)
dtm$trial <- trials$trial


set.seed(144)
split <- sample.split(dtm$trial,0.7)
train <- subset(dtm,split==T)
test <- subset(dtm,split==F)

prop.table(table(train$trial))

trial1 <- rpart(trial~.,train,method='class')
trial1
fancyRpartPlot(trial1)

predtrain <- predict(trial1)[,2]
max(predtrain)
confusionMatrix(predict(trial1,type='class'),train$trial,positive='1')

confusionMatrix(predict(trial1,test,type='class'),test$trial,positive='1')


library(ROCR)
predROCR <- prediction(predict(trial1,test)[,2],test$trial)
perf = performance(predROCR,'fnr','fpr')
plot(perf,colorize=T)

performance(predROCR,'auc')@y.values


##########
#SPAM HAM
emails <- read.csv('emails.csv',stringsAsFactors=F)
head(emails)
table(emails$spam)
max(nchar(emails$text))
which.min(nchar(emails$text))


corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,stopwords("english"))
corpus = tm_map(corpus,stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm
dtm1 <- removeSparseTerms(dtm,0.95)

emails1 <- as.data.frame(as.matrix(dtm1))
names(emails1) <- make.names(names(emails1))

which.max(colSums(emails1))

emails1$spam <- emails$spam

sum(colSums(emails1[emails1$spam==0,-331])>=5000)
sum(colSums(emails1[emails1$spam==1,-331])>=1000)

emails1$spam <- as.factor(emails1$spam)

set.seed(123)
split <- sample.split(emails1$spam,0.7)
train <- subset(emails1,split==T)
test <- subset(emails1,split==F)


spam_log <- glm(spam~.,train,family='binomial')
spam_cart <- rpart(spam~.,train,method='class')
set.seed(123)
spam_rf <- randomForest(spam~.,train)

predlog <- predict(spam_log,type='response')
predcart <- predict(spam_cart)[,2]
predrf <- predict(spam_rf,type='prob')[,2]

fancyRpartPlot(spam_cart)

table(train$spam,predlog>=0.5)

predROCR <- prediction(predlog,train$spam)
perf = performance(predROCR,'tpr','fpr')
plot(perf,colorize=T)
performance(predROCR,'auc')@y.values

confusionMatrix(predict(spam_cart,type='class'),train$spam)

predROCR <- prediction(predcart,train$spam)
perf = performance(predROCR,'tpr','fpr')
plot(perf,colorize=T)
performance(predROCR,'auc')@y.values


confusionMatrix(predict(spam_rf),train$spam)
predROCR <- prediction(predrf,train$spam)
perf = performance(predROCR,'tpr','fpr')
plot(perf,colorize=T)
performance(predROCR,'auc')@y.values


confusionMatrix(factor(as.numeric(predict(spam_log,test,type='response')>=0.5),c(0,1)),test$spam)$overall[1]
confusionMatrix(predict(spam_cart,test,type='class'),test$spam)$overall[1]
confusionMatrix(predict(spam_rf,test),test$spam)$overall[1]

performance(prediction(predict(spam_log,test,type='response'),test$spam),'auc')@y.values
performance(prediction(predict(spam_cart,test)[,2],test$spam),'auc')@y.values
performance(prediction(predict(spam_rf,test,type='prob')[,2],test$spam),'auc')@y.values

word_count <- rowSums(as.matrix(dtm))

emails2 <- emails1
emails2$word_cnt_log <- log(word_count)


train2 <- subset(emails2,split==T)
test2 <- subset(emails2,split==F)


spam_cart2 <- rpart(spam~.,train2,method='class')
set.seed(123)
spam_rf2 <- randomForest(spam~.,train2)

fancyRpartPlot(spam_cart2)

confusionMatrix(predict(spam_cart2,test2,type='class'),test2$spam)$overall[1]
confusionMatrix(predict(spam_rf2,test2),test$spam)$overall[1]

performance(prediction(predict(spam_cart2,test2)[,2],test2$spam),'auc')@y.values
performance(prediction(predict(spam_rf2,test2,type='prob')[,2],test2$spam),'auc')@y.values

#alternative way (simpler I think)
library(AUC)
auc(roc(predict(spam_cart2,test2)[,2],test2$spam))
plot((roc(predict(spam_cart2,test2)[,2],test2$spam)))



########
#try ngrams

library(tau)

#calculate the 2grams
docs <- data.frame(text=sapply(1:5728,function (x) unlist(corpus[[x]][[1]])),
                   stringsAsFactors=F)

ngrams2 <- sapply(docs$text,textcnt,method='string',n=2)
ngrams3 <- sapply(docs$text,textcnt,method='string',n=3)

#define  helper functions
mydf2 <- function(x) {
  y=ngrams2
  if (length(y[[x]])>0) {
    data.frame(ngram=names(y[[x]]),
               freq=as.numeric(y[[x]]),index=x,
               stringsAsFactors=F)
  }
}

mydf3 <- function(x) {
  y=ngrams3
  if (length(y[[x]])>0) {
    data.frame(ngram=names(y[[x]]),
               freq=as.numeric(y[[x]]),index=x,
               stringsAsFactors=F)
  }
}

#create a master df with all the 2grams and one with 3grams
library(dplyr)
library(tidyr)
tmp2 <- bind_rows(Map(mydf2,1:dim(docs)[1]))
tmp3 <- bind_rows(Map(mydf3,1:dim(docs)[1]))

#combine and modify to have columns for each var
ngrams <- bind_rows(tmp2,tmp3) %>% mutate(ngram=gsub(' ','_',ngram)) %>%
  group_by(index,ngram) %>% summarise(freq=sum(freq)) 

#keep only if in 5% or more
keep <- ngrams %>% group_by(ngram) %>% summarise(N=n()) %>% 
  filter(N>=ceiling(5728*.05)) %>% select(ngram)

ngrams_wide <- ngrams %>% filter(ngram %in% keep$ngram) %>% spread(ngram,freq,fill=0)
#we lost some indexes, as they do not have such words, we cn make those NAs, 0 after we merge
row.names(emails2) <- 1:5728

emails2$index = 1:5728

emails3 <- left_join(emails2,ngrams_wide,by='index')
emails3[is.na(emails3)] <- 0
emails3$spam <- emails1$spam
#Now I can use emails3 (-index) to predict and see if we do better

save.image('emails.rdata')


train3 <- subset(emails3,split==T)
test3 <- subset(emails3,split==F)

email_cart3 <- rpart(spam ~. - index,train3,method='class') 
fancyRpartPlot(email_cart3)
confusionMatrix(predict(email_cart3,type='class'),train3$spam)


test <- data.frame(var=c('top','<leaf>','right'),n=c(100,50,50),wt=c(100,50,50),yval=c(1,1,1))
