
movies <- read.table('http://files.grouplens.org/datasets/movielens/ml-100k/u.item',
                   sep='|',stringsAsFactors=T,fill=T,header=F,quote='\"')
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

movies$ID= NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

movies = unique(movies)

colSums(movies[-1])
sum(movies$Romance & movies$Drama)

distances <- dist(movies[-1],method='euclidean')
clustered_movies <- hclust(distances,method='ward')
plot(clustered_movies)
cluster_groups = cutree(clustered_movies,k=10)

library(dplyr)
data.frame(movies,group=cluster_groups) %>% group_by(group) %>% summarise_each(funs(mean),2:20) %>% gather(genre,value,-1) %>% spread(group,value)

subset(movies, Title =='Men in Black (1997)')
cluster_groups[257]
cluster2 = subset(movies,cluster_groups==2)
cluster2$Title[1:10]

plot(clustered_movies,labels=F,col=cluster_groups)


cluster_groups2 = cutree(clustered_movies,k=2)
data.frame(movies,group=cluster_groups2) %>% group_by(group) %>% 
  summarise_each(funs(mean),2:20) %>% gather(genre,value,-1) %>% spread(group,value)

####### Heart Attacks