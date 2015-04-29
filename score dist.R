scores <- read.csv('Documents/Edx/scores 20150428 835PM.csv')
scores1 <- na.omit(scores)
library(ggplot2)

ggplot(scores1, aes(x=Submissions,y=Score))+geom_point(color='blue')+
  theme_bw()

quantiles = quantile(scores1$Score,probs=c(0.5,0.75,0.9,0.95,0.99),na.rm=T)
ggplot(scores1,aes(x=Score))+geom_density(fill='cyan',alpha=0.3)+theme_bw()+
  geom_vline(xintercept=quantiles[4],color='red')+
  geom_vline(xintercept=quantiles[5],color='red')+
  geom_vline(xintercept=0.93525,color='blue')+
  coord_cartesian(xlim=c(0.7,0.95),ylim=c(0,25))

ggsave('scores.png',width = 5,height = 3,units="in")
