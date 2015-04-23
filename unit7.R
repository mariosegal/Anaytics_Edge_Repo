setwd('Documents/Analytics_edge/')
who <- read.csv('WHO.csv')

plot(who$GNI,who$FertilityRate)

library(ggplot2)

scatterplot = ggplot(who,aes(x=GNI,y=FertilityRate,color=LifeExpectancy,shape=Region))
fert_gni_plot = scatterplot+geom_point()+ggtitle('Fertility vs. GNI')
fert_gni_plot

ggplot(who,aes(x=log(FertilityRate),y=Under15,color=Region))+geom_point()+stat_smooth(method='lm',color='red',se=F)
model=lm(Under15~log(FertilityRate),who)
summary(model)
ggplot(who,aes(x=log(FertilityRate),y=Under15,color=Region))+scale_color_brewer(palette="Dark2")+geom_point(size=2)


#########
crime <- read.csv('mvtWeek1.csv')
crime$Date <- strptime(crime$Date ,'%m/%d/%y %H:%M')
crime$weekday <- weekdays(crime$Date)
crime$hour <- crime$Date$hour
crime$weekday <- factor(crime$weekday,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),ordered=T)

ggplot(crime,aes(x=weekday,group='identity'))+geom_line(stat='bin',alpha=0.3)

library(dplyr)
crime[-2] %>% group_by(weekday, hour) %>% summarise(N=n())  %>% ggplot(aes(x=hour,y=N,color=weekday)) +geom_line()
crime[-2] %>% group_by(weekday, hour) %>% summarise(N=n())  %>% ggplot(aes(y=weekday,x=hour,fill=N))+geom_tile()+
  scale_fill_gradient(name='Total MV Thefts',low='white',high='red') + theme(axis.title.y=element_blank())

install.packages('maps')
install.packages('ggmap')
library(maps);library(ggmap)

chicago = get_map(location= 'chicago',zoom=11)
ggmap(chicago)+geom_point(data=crime[1:100,],aes(x=Longitude,y=Latitude),col='red')

crime1 <- crime[-2] %>% group_by(long=round(Longitude,2), lat=round(Latitude,2)) %>% summarise(N=n()) 
ggmap(chicago)+geom_point(data=crime1,aes(x=long,y=lat,color=N,size=N))+
  scale_color_gradient(name='Thefts',low='yellow',high='red')


ggmap(chicago)+geom_tile(data=crime1,aes(x=long,y=lat,alpha=N),fill='red')

######

murders = read.csv('murders.csv')

statesMap = map_data("state")
ggplot(statesMap,aes(x=long,y=lat,group=group))+geom_polygon(fill='white',color='blue')
murders$region <- tolower(murders$State)

murdermap <- merge(statesMap,murders,by='region')
ggplot(murdermap,aes(x=long,y=lat,group=group,fill=Murders))+geom_polygon(color='black')+scale_fill_gradient(low='yellow',high='red',guide="legend")
ggplot(murdermap,aes(x=long,y=lat,group=group,fill=Population))+geom_polygon(color='black')+scale_fill_gradient(low='yellow',high='red',guide="legend")
ggplot(murdermap,aes(x=long,y=lat,group=group,fill=100000*Murders/Population))+geom_polygon(color='black')+scale_fill_gradient(low='white',high='red',guide="legend")
ggplot(murdermap,aes(x=long,y=lat,group=group,fill=100000*Murders/Population))+geom_polygon(color='black')+
  scale_fill_gradient(low='white',high='red',guide="legend",limits=c(0,10))
ggplot(murdermap,aes(x=long,y=lat,group=group,fill=GunOwnership))+geom_polygon(color='black')+
  scale_fill_gradient(low='white',high='red',guide="legend")
