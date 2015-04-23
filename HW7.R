library(ggplot2)
library(ggmaps)
library(maps)

statesMap = map_data("state")
length(unique(statesMap$group))

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

setwd('Documents/Analytics_edge/')
polling <- read.csv('PollingData_Imputed.csv')

Train <- subset(polling,Year<=2008)
Test <- subset(polling,Year==2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

sum(predictionDataFrame$TestPredictionBinary==1,na.rm=T)
mean(predictionDataFrame$TestPrediction,na.rm=T)


predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

subset(predictionDataFrame,Test.State=='Florida')


#tweets
tweets <- read.csv('tweets.csv')
library(tm)
library(SnowballC)

#tweets$negative <- as.factor(tweets$Avg<=-1)
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower) #convert to lower case
corpus = tm_map(corpus, PlainTextDocument)  #
corpus = tm_map(corpus,removePunctuation) #obvious
corpus = tm_map(corpus,removeWords,c('apple',stopwords("english"))) #remove stop words
#corpus = tm_map(corpus,stemDocument)  #stem words
frequencies <- DocumentTermMatrix(corpus)
alltweets = as.data.frame(as.matrix(frequencies))

library(wordcloud)
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(names(alltweets),colSums(alltweets),scale=c(2,0.25))

wordcloud(names(alltweets),colSums(alltweets[tweets$Avg<=-1,]),scale=c(2,0.25))

wordcloud(names(alltweets),colSums(alltweets),scale=c(2,0.25),random.order = F,color=brewer.pal(9,'YlOrRd')[-c(1:4)])


#parole
parole <- read.csv('parole.csv')

parole$male = as.factor(parole$male)

parole$state = as.factor(parole$state)

parole$crime = as.factor(parole$crime)

ggplot(data = parole, aes(x = age)) + geom_histogram()
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5,color='blue')

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(position="identity",alpha=0.5,binwidth = 5) + scale_fill_manual(values=colorPalette)


ggplot(data = parole, aes(x = time.served)) + geom_histogram(position="identity",alpha=0.5,binwidth = 1) + scale_fill_manual(values=colorPalette)

ggplot(data = parole, aes(x = time.served,fill=crime)) + geom_histogram(position="identity",alpha=0.2,binwidth = 1) 
ggplot(data = parole, aes(x = time.served,fill=crime)) + geom_density(position="identity",alpha=0.2,binwidth = 1) 


ggplot(data = parole, aes(x = time.served,fill=crime)) + 
  geom_histogram(position="identity",alpha=0.7,binwidth = 1) +facet_grid(crime~.)

