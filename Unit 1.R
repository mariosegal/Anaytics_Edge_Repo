

who <- read.csv('WHO.csv')

mean(who$Over60)

library(dplyr)
who %>% arrange(Over60) %>% select(Country:Over60) %>% slice(1:10)

who %>% arrange(desc(LiteracyRate)) %>% select(c(Country,LiteracyRate)) %>% slice(1:10)

tapply(who$ChildMortality,who$Region,mean)

who %>% group_by(Region) %>% summarise(ChildMortality=mean(ChildMortality)) %>% arrange(ChildMortality)


##########
### Recitation


usda <- read.csv("USDA.csv")
str(usda)
summary(usda)

which.max(usda$Sodium)
usda[which.max(usda$Sodium),]

high_sodium <- subset(usda, Sodium >10000)
nrow(high_sodium)
high_sodium$Description

match("CAVIAR",usda$Description)
usda[match("CAVIAR",usda$Description),'Sodium']

plot(usda$Protein,usda$TotalFat,xlab='Protein',ylab='Fat',main='Protein vs. Fat',col='red')
hist(usda$VitaminC,xlab='Vitamin C',main='Histogram of Vitamn C',xlim=c(0,100),col='blue',breaks=2000)

boxplot(usda$Sugar,main='Boxplot of Sugar levels',ylab='Sugar in Grams')


usda$high_sodium <- as.numeric(usda$Sodium >= mean(usda$Sodium,na.rm=T),na.rm=T)
usda$high_fat <- as.numeric(usda$TotalFat >= mean(usda$TotalFat,na.rm=T),na.rm=T)
usda$high_protein <- as.numeric(usda$Protein >= mean(usda$Protein,na.rm=T),na.rm=T)
usda$high_carbs <- as.numeric(usda$Carbohydrate >= mean(usda$Carbohydrate,na.rm=T),na.rm=T)
table(usda$high_sodium,usda$high_fat)


tapply(usda$Iron,usda$high_protein,mean,na.rm=T)
tapply(usda$VitaminC,usda$high_carbs,max,na.rm=T)
tapply(usda$VitaminC,usda$high_carbs,summary)

library(ggplot2)

ggplot(subset(usda,!is.na(high_carbs)),aes(x=factor(high_carbs),y=VitaminC,fill=factor(high_carbs)))+geom_boxplot()+
  geom_jitter(color='blue',position=position_jitter(width=0.2),alpha=0.5,shape='.')+
  coord_cartesian(ylim=c(0,25))


ggplot(subset(usda,!is.na(high_fat)),aes(x=factor(high_fat),y=Sugar,fill=factor(high_fat)))+geom_boxplot()+
  geom_jitter(color='blue',position=position_jitter(width=0.2),alpha=0.5,shape='.')+
  coord_cartesian(ylim=c(0,10))


###########

cps <- read.csv('CPSData.csv')
summary(cps)
str(cps)


cps %>% group_by(Industry) %>% summarise(N=n()) %>% arrange(desc(N))

sort(table(cps$State))

prop.table(table(cps$Citizenship))

(table(cps$Race,cps$Hispanic))

which(apply(cps,2,function(x) sum(is.na(x))>0))

(table(is.na(cps$Married),cps$Region))

prop.table(table(is.na(cps$Married),cps$Sex),1)

prop.table(table(is.na(cps$Married),cps$Citizenship),1)

prop.table(table(is.na(cps$Married),cps$Age),1)

prop.table(table(is.na(cps$Married),cut(cps$Age,c(0,18,25,35,45,55,65,75,Inf))),1)

prop.table(table(cps$State,is.na(cps$MetroAreaCode)),1)


prop.table(table(cps$Region,is.na(cps$MetroAreaCode)),1)

sort(tapply(is.na(cps$MetroAreaCode),cps$State,mean))




metros <- read.csv('MetroAreaCodes.csv')
countries <-read.csv('CountryCodes.csv')

cps1 <- merge(cps,metros,by.x='MetroAreaCode',by.y='Code',all.x=T)

sort(table(cps1$MetroArea))

sort(tapply(cps1$Hispanic,cps1$MetroArea,mean))

which(tapply(cps1$Race=='Asian',cps1$MetroArea,mean)>=0.2)


cps1 %>% group_by(MetroArea,Education) %>% summarise(N=n()) %>% group_by(MetroArea) %>%
  mutate(P=N/sum(N)) %>% filter(Education=="No high school diploma") %>% ungroup() %>%
  arrange((P))

cps1 <- merge(cps1,countries,by.x='CountryOfBirthCode',by.y='Code',all.x=T)
sum(is.na(cps1$Country))

cps1 %>% group_by(Country) %>% summarise(N=n()) %>% arrange(desc(N))

cps1 %>% filter(MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA" & !is.na(Country)) %>% 
  group_by(Country) %>% summarise(N=n()) %>% mutate(P=N/sum(N)) %>% arrange(desc(P))


sort(tapply(cps1$Country=='India',cps1$MetroArea,sum,na.rm=T))


sort(tapply(cps1$Country=='Brazil',cps1$MetroArea,sum,na.rm=T))

sort(tapply(cps1$Country=='Somalia',cps1$MetroArea,sum,na.rm=T))
