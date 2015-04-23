#Recitation 6

flower <- read.csv('flower.csv',header=F)
flower_mat <- as.matrix(flower)

flower_vec <- as.vector(flower_mat)
distance = dist(flower_vec,method="euclidean")
cluster1 <- hclust(distance,method='ward')
plot(cluster1)
rect.hclust(cluster1,k=3,border='red')
flower_clust = cutree(cluster1,k=3)
tapply(flower_vec,flower_clust,mean)
par(mfrow=c(1,2))
image(flower_mat,axes=F,main='original',col=grey(seq(0,1,length=256)))
image(matrix(flower_clust,byrow=T,nrow=50),axes=F,main='segmented')

#this was cool, I guess we used te intensity to define regions, on the flower they mean
#nothing but likely on a tumor, or a map, or something they have meaning

healthy <- read.csv('healthy.csv',header=F)
healthy1 <- as.matrix(healthy)
par(mfrow=c(1,1))
image(healthy1,axes=F,col=grey(seq(0,1,length=256)))
healthy_vec = as.vector(healthy1)
#healthy_dist = dist(healthy_vec,method='euclid')

#kmeans
k = 5
set.seed(1)
kmc = kmeans(healthy_vec,centers=k,iter.max=1000)
str(kmc)
healthy_clust = kmc$cluster
kmc$centers  #mean intensity
kmc$size

par(mfrow=c(1,2))
image(healthy1,axes=F,col=grey(seq(0,1,length=256)))
image(matrix(healthy_clust,byrow=F,nrow=566),axes=F,col=rainbow(k))

tumor <- read.csv('tumor.csv',header=F)
tumor1 <- as.matrix(tumor)
tumor_vec <- as.vector(tumor1)

install.packages('flexclust')
library(flexclust)

kmc.kcca = as.kcca(kmc,healthy_vec)
tumor_clust = predict(kmc.kcca,newdata=tumor_vec)
image(matrix(healthy_clust,byrow=F,nrow=566),axes=F,col=rainbow(k),main='healthy')
image(matrix(tumor_clust,byrow=F,nrow=571),axes=F,col=rainbow(k),main='tumor')

