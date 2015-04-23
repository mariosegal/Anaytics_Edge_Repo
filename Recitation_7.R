mit <- read.csv('intl.csv')
library(scales)

mit = transform(mit,Region=reorder(Region,-PercentOfIntl))
ggplot(mit,aes(x=Region,y=PercentOfIntl,fill=Region))+geom_bar(stat='identity')+
  geom_text(aes(label=PercentOfIntl*100),vjust=-0.4)+
  theme(legend.position='none',axis.text.x=element_text(angle=45,hjust=1))+
  ylab('% International Students')+
  xlab("")
  
ggplot(mit,aes(x='1',y=PercentOfIntl,fill=Region))+geom_bar(stat='identity',position='stack')
  geom_text(aes(label=PercentOfIntl*100),vjust=-0.4)+
  theme(legend.position='none',axis.text.x=element_text(angle=45,hjust=1))+
  ylab('% International Students')+
  xlab("")

library(ggplot2)
library(ggmap)
mit1 <- read.csv('intlall.csv',stringsAsFactors=F)
head(mit1)
mit1[is.na(mit1)] <- 0

world_map <- map_data('world')
head(world_map)

world_map1 <- merge(world_map,mit1,by.x='region',by.y='Citizenship',all.x=T)
world_map1 = world_map1[order(world_map1$group,world_map1$order),]
ggplot(world_map1,aes(x=long,y=lat,group=group,fill=Total))+geom_polygon(color='black')+
  coord_map(projection='mercator')

ggplot(world_map,aes(x=long,y=lat,group=group))+geom_polygon(color='black')+
  coord_map(projection='mercator')

