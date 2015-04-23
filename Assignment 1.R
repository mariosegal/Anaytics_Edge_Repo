

data <- read.csv('mvtWeek1.csv')
summary(data)

head(data$Date)


DateConvert = as.Date(strptime(data$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

data$month <- months(DateConvert)
data$weekday <- weekdays(DateConvert)

data$Date <- as.Date(strptime(data$Date, "%m/%d/%y %H:%M"))

which.min(table(data$month))
which.max(table(data$weekday))

table(data$month,data$Arrest)


hist(data$Date, breaks=100)
data %>% group_by(period=format(data$Date,'%Y%m')) %>% summarise(N=n()) %>%
  ggplot(aes(y=N,x=period,group='identity'))+
  geom_bar(fill='cyan',stat='identity',alpha=0.2)+geom_line(color='red')

ggplot(data,aes(x=Arrest,y=Date,fill=Arrest))+geom_boxplot()

data %>% group_by(period=format(data$Date,'%Y%m'),Arrest) %>% summarise(N=n()) %>%
  ggplot(aes(y=N,x=period,color=Arrest,group=Arrest))+
  geom_line()+facet_grid(Arrest~.,scales='free')

prop.table(table(format(data$Date,'%Y'),data$Arrest),1)

aux <- as.data.frame(prop.table(table(format(data$Date,'%Y'),data$Arrest),1))
ggplot(aux,aes(x=Var1,y=Freq,color=Var2,group=Var2))+geom_line()


data %>% group_by(LocationDescription) %>% summarise(N=n()) %>% arrange(desc(N)) %>% 
  filter(LocationDescription != 'OTHER') %>% slice(1:5) %>% summarise(N1=sum(N))

top_locs <-  data %>% group_by(LocationDescription) %>% summarise(N=n()) %>% arrange(desc(N)) %>% 
  filter(LocationDescription != 'OTHER') %>% slice(1:5) %>% select(LocationDescription)

data %>% filter(LocationDescription %in% top_locs$LocationDescription) %>% 
  group_by(LocationDescription,Arrest) %>% summarise(N=n()) %>% group_by(LocationDescription) %>%
  mutate(p=N/sum(N)) %>% gather(var,value,N:p) %>% unite(key,Arrest,var) %>% 
  spread(key,value) 

data %>% filter(LocationDescription== 'GAS STATION') %>% 
  group_by(weekday,Arrest) %>% summarise(N=n()) %>% group_by(weekday) %>%
  mutate(p=N/sum(N)) %>% gather(var,value,N:p) %>% unite(key,Arrest,var) %>% 
  spread(key,value) %>% mutate(tot=FALSE_N+TRUE_N) %>% arrange(desc(tot))


data %>% filter(LocationDescription== 'DRIVEWAY - RESIDENTIAL') %>% 
  group_by(weekday,Arrest) %>% summarise(N=n()) %>% group_by(weekday) %>%
  mutate(p=N/sum(N)) %>% gather(var,value,N:p) %>% unite(key,Arrest,var) %>% 
  spread(key,value) %>% mutate(tot=FALSE_N+TRUE_N) %>% arrange(desc(tot))


#stock analysis

boeing <- read.csv('BoeingStock.csv')
ge <- read.csv('GEStock.csv')
procter <- read.csv('ProcterGambleStock.csv')
ibm <- read.csv('IBMStock.csv')
ko <- read.csv('CocaColaStock.csv')

boeing$stock <- 'Boeing'
ko$stock <- 'Coke'
ge$stock <- 'GE'
ibm$stock <- 'IBM'
procter$stock <- 'P&G'

stocks <- rbind(boeing,ko,ge,ibm,procter)
stocks$Date <- as.Date(stocks$Date, "%m/%d/%y")
min(stocks$Date)
max(stocks$Date)


tapply(stocks$StockPrice,stocks$stock,mean)
tapply(stocks$StockPrice,stocks$stock,min)
tapply(stocks$StockPrice,stocks$stock,summary)
tapply(stocks$StockPrice,stocks$stock,sd)


ggplot(subset(stocks,stock=='Coke'),aes(x=Date,y=StockPrice,color=stock))+geom_line()+facet_grid(stock~.)

ggplot(subset(stocks,stock %in% c('Coke','P&G')),aes(x=Date,y=StockPrice,color=stock))+
  geom_line()


ggplot(subset(stocks,Date>'1995-01-01' & Date < '2005-01-01'),aes(x=Date,y=StockPrice,color=stock))+
  geom_line()+geom_vline(xintercept=as.numeric(as.Date('1997-09-01')),color='red',linetype=2)+
  geom_vline(xintercept=as.numeric(as.Date('1997-11-01')),color='red',linetype=2)

ggplot(subset(stocks,Date>'1997-01-01' & Date < '1998-01-01'),aes(x=Date,y=StockPrice,color=stock))+
  geom_line()+geom_vline(xintercept=as.numeric(as.Date('1997-09-01')),color='red',linetype=2)+
  geom_vline(xintercept=as.numeric(as.Date('1997-11-01')),color='red',linetype=2)

ggplot(subset(stocks,Date>'2004-01-01' & Date < '2007-01-01'),aes(x=Date,y=StockPrice,color=stock))+
  geom_line()

stocks %>% filter(stock=='IBM') %>% group_by(month=months(Date),stock) %>% 
  summarise(avg=mean(StockPrice)) %>% mutate(check=(avg>ibm_mean[2]))
 

ibm_mean <- stocks %>% filter(stock=='IBM') %>% group_by(stock) %>% summarise(avg=mean(StockPrice)) 

stocks %>%  group_by(month=months(Date),stock) %>% 
  summarise(avg=mean(StockPrice)) %>% spread(stock,avg)
