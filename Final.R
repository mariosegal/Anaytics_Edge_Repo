
Airlines = read.csv('Documents/Edx/AirlineDelay.csv')

set.seed(15071)

spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]

AirlinesTest = Airlines[-spl,]

delay1 = lm(TotalDelay~.,data=AirlinesTrain)
summary(delay1)

cor(AirlinesTrain$NumPrevFlights,AirlinesTrain$PrevFlightGap)
cor(AirlinesTrain$OriginAvgWind,AirlinesTrain$OriginWindGust)

pred1 = predict(delay1,newdata=AirlinesTest)

sse = sum((AirlinesTest$TotalDelay-pred1)^2)
sse
sst = sum((AirlinesTest$TotalDelay-mean(AirlinesTrain$TotalDelay))^2)
sst

1-(sse/sst)


Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))

sum(Airlines$TotalDelay ==0)
sum(Airlines$TotalDelay >0 & Airlines$TotalDelay<30)
sum(Airlines$TotalDelay >=30)

table(Airlines$DelayClass )

Airlines$TotalDelay = NULL
AirlinesTrain$DelayClass = factor(ifelse(AirlinesTrain$TotalDelay == 0, "No Delay", ifelse(AirlinesTrain$TotalDelay >= 30, "Major Delay", "Minor Delay")))

AirlinesTest$DelayClass = factor(ifelse(AirlinesTest$TotalDelay == 0, "No Delay", ifelse(AirlinesTest$TotalDelay >= 30, "Major Delay", "Minor Delay")))

set.seed(15071)
spl1 = sample.split(Airlines$DelayClass,.7)
AirlinesTrain = subset(Airlines,spl1==T)
AirlinesTest= subset(Airlines,spl1==F)

library(rpart)
tree1 = rpart(DelayClass ~ . , data=AirlinesTrain)
tree1
plot(tree1)
text(tree1)

treep = predict(tree1,newdata=AirlinesTest,type='class')
table(AirlinesTest$DelayClass,treep)
table(AirlinesTest$DelayClass)


######

ebay = read.csv('Documents/Edx/ebay.csv',stringsAsFactors=F)
prop.table(table(ebay$sold))
which(sapply(ebay,function(x) sum(is.na(x))>0))
which.max(prop.table(table(ebay$size)))


ebay$sold = as.factor(ebay$sold)
ebay$condition = as.factor(ebay$condition)
ebay$heel = as.factor(ebay$heel)
ebay$style = as.factor(ebay$style)
ebay$color = as.factor(ebay$color)
ebay$material = as.factor(ebay$material)


set.seed(144)

library(caTools)

spl = sample.split(ebay$sold, 0.7)

training = subset(ebay,spl==T)
testing = subset(ebay,spl==F)

log1 = glm(sold~biddable+startprice+condition+heel+style+color+material,data=training,family='binomial')
summary(log1)


1/(1+exp(-(100*-0.0044423 + -0.4952981+0.122426+0.5268920+0.2226547-1.1078098)))

table(testing$sold,predict(log1,newdata=testing,type='response')>=0.5)
sum(factor(ifelse(predict(log1,newdata=testing,type='response')<0.5,0,1),c(0,1))!=factor(rep(0,1139),c(0,1)))

install.packages('ROCR')
library(ROCR)
predictTest = predict(log1, type="response", newdata=training)
ROCRpredTest = prediction(predictTest, training$sold)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
plot(performance(ROCRpredTest,'tpr','fpr'),colorize=T,print.cutoffs.at=c(seq(0.1,1,by=0.1)))


library(caret)
numflds <- trainControl(method='cv',number=10)
cpgrid = expand.grid(.cp=seq(.001,.05,length.out = 50))
set.seed(144)
shoe_tree= train(sold~biddable+startprice+condition+heel+style+color+material,training,method='rpart',
                 trControl = numflds, tuneGrid = cpgrid)
shoe_tree
shoe_tree1 = rpart(sold~biddable+startprice+condition+heel+style+color+material,data=training,cp=0.005,method='class')


########
library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(ebay$description))
corpus = tm_map(corpus,tolower) #convert to lower case
corpus = tm_map(corpus, PlainTextDocument)  #
corpus = tm_map(corpus,removePunctuation) #obvious
corpus = tm_map(corpus,removeWords,stopwords("english")) #remove stop words and apple
corpus = tm_map(corpus,stemDocument)  #stem words

dtm <- DocumentTermMatrix(corpus)
dtm
spdtm = removeSparseTerms(dtm,0.9)
spdtm
descriptionText = as.data.frame(as.matrix(spdtm))
names(descriptionText) = paste0("D", names(descriptionText))

descriptionText = cbind(descriptionText,ebay[c('sold','biddable','startprice','condition','heel','style','color','material')])

trainText = subset(descriptionText,spl==T)
testText = subset(descriptionText,spl==F)

glmText = glm(sold~.,data=trainText,family='binomial')
which(summary(glmText)$coeff[-1,4] < 0.05)


predictTest = predict(glmText, type="response", newdata=trainText)
ROCRpredTest = prediction(predictTest, trainText$sold)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


predictTest1 = predict(glmText, type="response", newdata=testText)
ROCRpredTest1 = prediction(predictTest1, testText$sold)
auc1 = as.numeric(performance(ROCRpredTest1, "auc")@y.values)
auc1


######
hubway = read.csv('Documents/Edx/HubwayTrips.csv')
mean(hubway$Duration)
tapply(hubway$Duration,hubway$Weekday,mean)
table(hubway$Morning)
table(hubway$Afternoon)
table(hubway$Evening)
prop.table(table(hubway$Male))


library(caret)
preproc = preProcess(hubway)
HubwayNorm = predict(preproc, hubway)
summary(HubwayNorm)

set.seed(5000)
km1 = kmeans(HubwayNorm,10)
table(km1$cluster)


library(dplyr)
library(tidyr)
data = cbind(hubway,km1$cluster)
names(data)[8] = "cluster"
 res1= data %>% group_by(cluster) %>% summarise_each(funs(mean),-cluster)
View(res1)


set.seed(8000)
km2 = kmeans(HubwayNorm,20)
km2$size


data1 = cbind(hubway,km2$cluster)
names(data1)[8] = "cluster"
res2= data1 %>% group_by(cluster) %>% summarise_each(funs(mean),-cluster)
View(res2)


#########
obj = rep(0,20)
obj[seq(2,20,2)] = 1

rhs = c(rep(40,5),rep(80,5),2450,2100,2800,2800,1960)
dir = c(rep('<=',10),rep('>=',5))
con = matrix(0,nrow=15,ncol=20)
con[1,c(2,4)] = 1
con[2,c(6,8)] = 1
con[3,c(10,12)] = 1
con[4,c(14,16)] = 1
con[5,c(18,20)] = 1

con[6,c(1,3)] = 1
con[7,c(5,7)] = 1
con[8,c(9,11)] = 1
con[9,c(13,15)] = 1
con[10,c(17,19)] = 1

con[11,c(1,2,5,6)] = c(40,40,35,35)
con[12,c(7:10,13:14)] = c(27,25,30,30,35,35)
con[13,c(15:16)] = c(50,50)
con[14,c(3,4,17,18)] = 60
con[15,c(11,12,19,20)] = c(45,45,50,50)


library(Rglpk)
solg= Rglpk_solve_LP(obj,con,dir,rhs,types=rep('C',20),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg 

rhs1=rhs
rhs1[c(1,6)]=0
solg1= Rglpk_solve_LP(obj,con,dir,rhs1,types=rep('C',20),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg1

rhs2=rhs
rhs2[c(2,7)]=0
solg2= Rglpk_solve_LP(obj,con,dir,rhs2,types=rep('C',20),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg2

rhs3=rhs
rhs3[c(3,8)]=0
solg3= Rglpk_solve_LP(obj,con,dir,rhs3,types=rep('C',20),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg3
con[11:15,] %*% (solg3$solution)

rhs4=rhs
rhs4[c(4,9)]=0
solg4= Rglpk_solve_LP(obj,con,dir,rhs4,types=rep('C',20),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg4
con[11:15,] %*% (solg4$solution)



rhs5=rhs
rhs5[c(5,10)]=0
solg5= Rglpk_solve_LP(obj,con,dir,rhs5,types=rep('C',20),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg5
con[11:15,] %*% (solg5$solution)

con1=con

con1[11,c(1,2,5,6)] = c(40,40,35,35)*.6
con1[12,c(7:10,13:14)] = c(27,25,30,30,35,35)*.55
con1[13,c(15:16)] = c(50,50)*.75
con1[14,c(3,4,17,18)] = 60*.65
con1[15,c(11,12,19,20)] = c(45,45,50,50)*.6


sol1 = solveLP( obj, rhs, con, maximum=F,const.dir=dir )
sol2 = solveLP( obj, rhs, con1, maximum=F,const.dir=dir )
