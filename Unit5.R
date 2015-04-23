tweets <- read.csv('tweets.csv')
library(tm)
library(SnowballC)

tweets$negative <- as.factor(tweets$Avg<=-1)
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower) #convert to lower case
corpus = tm_map(corpus, PlainTextDocument)  #
corpus = tm_map(corpus,removePunctuation) #obvious
corpus = tm_map(corpus,removeWords,c('apple',stopwords("english"))) #remove stop words and apple
corpus = tm_map(corpus,stemDocument)  #stem words

frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])  #data looks sparse
findFreqTerms(frequencies,lowfreq = 20)

sparse = removeSparseTerms(frequencies,0.995)
sparse

tweets_sparse <- as.data.frame(as.matrix(sparse))
colnames(tweets_sparse) = make.names(colnames(tweets_sparse))

tweets_sparse$negative <- tweets$negative

library(caTools)
set.seed(123)
split <- sample.split(tweets_sparse$negative,SplitRatio=0.7)
train_sparse  = subset(tweets_sparse,split==T)
test_sparse  = subset(tweets_sparse,split==F)

#prredict the sentiment
tweet_cart <- rpart(negative ~ ., train_sparse,method='class')
fancyRpartPlot(tweet_cart)
mean(test_sparse$negative==predict(tweet_cart,newdata=test_sparse,type='class'))


library(randomForest)
set.seed(123)

tweet_rf <- randomForest(negative ~ ., train_sparse)
mean(test_sparse$negative==predict(tweet_rf,newdata=test_sparse))
varImpPlot(tweet_rf)


tweet_log <- glm(negative ~ ., train_sparse,family='binomial')
mean(test_sparse$negative==(predict(tweet_log,newdata=test_sparse,type='response')>=0.5))



save.image('unit5.rdata')





