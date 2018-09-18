
install.packages("factoextra")
library(factoextra)
data<-read.csv("C:/Users/user/Desktop/Repos500.csv")
data1<-na.omit(data)
data1<-data1[,-c(1,2,3,4,5,6,7)]
data1<-data1[sample(nrow(data1),size=30000,replace=FALSE),]
i=0
for(i in c(1:30000)){
  if(data1[i,2]==0||data1[i,3]==0||data1[i,2]>1000||data1[i,3]>1000){
    data1[i,2]<-NA;
  }
  if(data1[i,1]!="JavaScript"&&data1[i,1]!="CSS"&&data1[i,1]!="HTML"&&data1[i,1]!="C"&&data1[i,1]!="C++"&&data1[i,1]!="Python"){
    data1[i,1]<-NA;
  }
}
data1<-na.omit(data1)
#write.csv(data1,file="t.csv")

data<-read.csv("C:/Users/user/Desktop/t.csv")

data<-data[,-1]

data1<-na.omit(data)
data2 <- data1[, -1] 

set.seed(20000)
kmeans.cluster <- kmeans(data2, centers=5,nstart=25) 
table(kmeans.cluster$cluster, data1$language) 
require(factoextra)
fviz_cluster(kmeans.cluster, data = data2, geom = c("point"),frame.type = "norm")   
plot.kmeans(kmeans.cluster, data = data1, class = "language")


if (!require(useful))
{  
  install.packages("useful")
  library(useful)
}

kv <- FitKMeans(data2, max.clusters=25, nstart=25, seed=20000)
PlotHartigan(kv)
kv

set.seed(20000)
kmeans.cluster <- kmeans(data2, centers=11,nstart=25) 
kmeans.cluster
table(kmeans.cluster$cluster, data1$language) 
fviz_cluster(kmeans.cluster, data = data2, geom = c("point"),frame.type = "norm")    
plot.kmeans(kmeans.cluster, data = data1, class = "language",frame=TRUE)

set.seed(20000)
kmeans.cluster <- kmeans(data2, centers=8,nstart=25) 
kmeans.cluster$withinss
table(kmeans.cluster$cluster, data1$language) 
fviz_cluster(kmeans.cluster, data = data2, geom = c("point"),frame.type = "norm")   
plot.kmeans(kmeans.cluster, data = data1, class = "language",frame=TRUE)

set.seed(20000)
kmeans.cluster <- kmeans(data2, centers=6,nstart=25) 
kmeans.cluster$withinss
table(kmeans.cluster$cluster, data1$language) 
fviz_cluster(kmeans.cluster, data = data2, geom = c("point"),frame.type = "norm")   
plot.kmeans(kmeans.cluster, data = data1, class = "language",frame=TRUE)

E.dist <- dist(data2, method="euclidean")

par(mfrow=c(1,1)) 

h.E.cluster <- hclust(E.dist)
plot(h.E.cluster)

hclust(E.dist, method="ward.D2")
h.cluster <- hclust(E.dist, method="ward.D2") 

plot(h.cluster)
abline(h=9, col="red")
