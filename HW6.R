kos <- read.csv('dailykos.csv')

kos_dist <- dist(kos,method='euclidean') 
kos_hc <- hclust(kos_dist,method='ward.D')
plot(kos_hc)

kos_clust <- cutree(kos_hc,7)
table(kos_clust)

View(cbind(kos,kos_clust) %>% group_by(kos_clust) %>% gather(word,value,-kos_clust) %>% 
  group_by(kos_clust,word) %>% summarise(count=sum(value)) %>% group_by(kos_clust) %>%
  arrange(desc(count)) %>% top_n(n=6))

set.seed(1000)
kos_km <- kmeans(kos,7)
kos_km$size

View(data.frame(kos,clust=kos_km$cluster) %>% group_by(clust) %>% gather(word,value,-clust) %>% 
       group_by(clust,word) %>% summarise(count=sum(value)) %>% group_by(clust) %>%
       arrange(desc(count)) %>% top_n(n=6))

table(kos_clust,kos_km$cluster)

pc <- princomp(kos)
library(ggplot2)
ggplot(NULL) +geom_point(aes(x=pc$scores[,1],y=pc$scores[,2],color=factor(kos_km$cluster),shape=factor(kos_clust)))

ggplot(NULL) +geom_point(aes(x=pc$scores[,6],y=pc$scores[,2],color=factor(kos_km$cluster),shape=factor(kos_clust)))


######
#AIRLINES

airlines <- read.csv('AirlinesCluster.csv')
summary(airlines)

library(caret)
params1 <- preProcess(airlines)
airlines_norm <- predict(params1,airlines)

airlines_dist <- dist(airlines_norm,method='euclidean')
airlines_hclust <- hclust(airlines_dist,method='ward.D')
plot(airlines_hclust)

airlines_clust <- cutree(airlines_hclust,5)
table(airlines_clust)

cbind(airlines,airlines_clust) %>% group_by(airlines_clust) %>% summarise_each(funs(mean))

set.seed(88)
airlines_km <- kmeans(airlines_norm,5,iter.max=1000)
airlines_km$size
table(airlines_clust,airlines_km$cluster)

ggplot(airlines) + geom_point(aes(x=DaysSinceEnroll,y=FlightTrans,color=as.factor(airlines_km$cluster),shape=as.factor(airlines_clust)))


#STOCKS

stocks <- read.csv('StocksCluster.csv')
c1 <- as.array(cor(stocks[1:11]))
diag(c1) <- NA
which(abs(c1)==max(abs(c1),na.rm=T),arr.ind=T)
which.max(colMeans(stocks[-12]))
which.min(colMeans(stocks[-12]))

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

log1 <- glm(PositiveDec~.,stocksTrain,family='binomial')
confusionMatrix(as.numeric(predict(log1,type='response')>=0.5),stocksTrain$PositiveDec,positive='1')
confusionMatrix(as.numeric(predict(log1,newdata=stocksTest,type='response')>=0.5),stocksTest$PositiveDec,positive='1')$overall[1]
prop.table(table(stocksTest$PositiveDec))


limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

set.seed(144)
km <- kmeans(normTrain,3)


library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)


stocks_train1 <- subset(stocksTrain,clusterTrain==1)
stocks_train2<- subset(stocksTrain,clusterTrain==2)
stocks_train3 <- subset(stocksTrain,clusterTrain==3)

stocks_test1 <- subset(stocksTest,clusterTest==1)
stocks_test2<- subset(stocksTest,clusterTest==2)
stocks_test3 <- subset(stocksTest,clusterTest==3)

log1a <- glm(PositiveDec~.,stocks_train1,family='binomial')
log2a <- glm(PositiveDec~.,stocks_train2,family='binomial')
log3a <- glm(PositiveDec~.,stocks_train3,family='binomial')

summary(log1a)
summary(log2a)
summary(log3a)


confusionMatrix(as.numeric(predict(log1a,newdata=stocks_test1,type='response')>=0.5),stocks_test1$PositiveDec,positive='1')$overall[1]
confusionMatrix(as.numeric(predict(log2a,newdata=stocks_test2,type='response')>=0.5),stocks_test2$PositiveDec,positive='1')$overall[1]
confusionMatrix(as.numeric(predict(log3a,newdata=stocks_test3,type='response')>=0.5),stocks_test3$PositiveDec,positive='1')$overall[1]


AllPredictions = c(predict(log1a,newdata=stocks_test1,type='response'),
predict(log2a,newdata=stocks_test2,type='response'),
predict(log3a,newdata=stocks_test3,type='response'))
AllOutcomes = c(stocks_test1$PositiveDec,stocks_test2$PositiveDec,stocks_test3$PositiveDec)
confusionMatrix(as.numeric(AllPredictions>=0.5),AllOutcomes)$overall[1]
