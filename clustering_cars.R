library(readxl)
library(MASS)
library(ggplot2)
car_data = read.csv('C:/Users/Pratyush Yadav/Desktop/IITM/Courses_Committees/Multivariate/Exam/Endterm data_2020/used_car_data.csv')
str(car_data)

boxplot(scale(car_data$Price..INR.))
hist(car_data$Mileage)

car_data[,7] = as.factor(car_data[,7])
car_data['Log_Price'] = log(car_data$Price..INR.)
car_data$Price..INR. = scale(car_data$Price..INR.,,scale=T)
car_data$Mileage = scale(car_data$Mileage,,scale=T)

for (i in 6:17){
  car_data[,i] = as.factor(car_data[,i])
}
car_data$Price..INR. = car_data$Price..INR.[,]
car_data$Mileage = car_data$Mileage[,]
library(cluster)
daisy(car_data[,-c(1,2)])
godist = daisy(car_data[,-c(1,2)],metric = 'gower')

fit = hclust(godist,method="complete")
plot(fit,cex=0.3)
abline(h=0.25,col='red')

library(fpc)
K=nrow(9)
for (i in 2:10){
  cut_avg = cutree(fit,i)
  k=cluster.stats(godist,cut_avg)
  K[i-1]=k$wb.ratio
}
plot(2:10,K,type='b',xlab='Number of Clusters',ylab='W/B Ratio')
for (i in 2:10){
  cut_avg = cutree(fit,i)
  k=cluster.stats(godist,cut_avg)
  print(k$ch)
}

for (i in 2:10){
  cut_avg = cutree(fit,i)
  k=cluster.stats(godist,cut_avg)
  K[i-1] = k$within.cluster.ss
}
plot(2:10,K,type='b',xlab='Number of Clusters',ylab='Within SS')

for (i in 2:10){
  cut_avg = cutree(fit,i)
  k=cluster.stats(godist,cut_avg)
  K[i-1] = k$avg.silwidth
}
plot(2:10,K,type='b',xlab='Number of Clusters',ylab='Average Silhouette Width')

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
cstats.table(godist,fit,8)

plot(fit,cex=0.01)
?groups <- cutree(fit, k=) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=6, border=2:6)
cut_avg <- cutree(fit, 6)
cut_avg

fviz_cluster()
library(stats)
identify(fit,N=10)

car_data['cluster'] = cut_avg

car_data

plot(table(car_data[,6]))
apply(car_data[,6],2,table)

table(car_data[,'cluster'])

cl1 = car_data[which(car_data$cluster==1),]
cl2 = car_data[which(car_data$cluster==2),]
cl3 = car_data[which(car_data$cluster==3),]
cl4 = car_data[which(car_data$cluster==4),]
cl5 = car_data[which(car_data$cluster==5),]
cl6 = car_data[which(car_data$cluster==6),]

data = data.frame()
data = rbind(data,apply(car_data[,3:5],2,max))
data = rbind(data,apply(car_data[,3:5],2,min))
data = rbind(data,apply(cl1[,3:5],2,mean))
data = rbind(data,apply(cl2[,3:5],2,mean))
data = rbind(data,apply(cl3[,3:5],2,mean))
data = rbind(data,apply(cl4[,3:5],2,mean))
data = rbind(data,apply(cl5[,3:5],2,mean))
data = rbind(data,apply(cl6[,3:5],2,mean))
colnames(data) = colnames(car_data[,3:5])


data1 = data.frame()
data1 = rbind(data1,apply(car_data[,3:5],2,max))
data1 = rbind(data1,apply(car_data[,3:5],2,min))
data1 = rbind(data1,apply(cl1[,3:5],2,mean))
colnames(data1) = colnames(car_data[,3:5])

data2 = data.frame()
data2 = rbind(data2,apply(car_data[,3:5],2,max))
data2 = rbind(data2,apply(car_data[,3:5],2,min))
data2 = rbind(data2,apply(cl2[,3:5],2,mean))
colnames(data2) = colnames(car_data[,3:5])

data3 = data.frame()
data3 = rbind(data3,apply(car_data[,3:5],2,max))
data3 = rbind(data3,apply(car_data[,3:5],2,min))
data3 = rbind(data3,apply(cl3[,3:5],2,mean))
colnames(data3) = colnames(car_data[,3:5])

data4 = data.frame()
data4 = rbind(data4,apply(car_data[,3:5],2,max))
data4 = rbind(data4,apply(car_data[,3:5],2,min))
data4 = rbind(data4,apply(cl4[,3:5],2,mean))
colnames(data4) = colnames(car_data[,3:5])

data5 = data.frame()
data5 = rbind(data5,apply(car_data[,3:5],2,max))
data5 = rbind(data5,apply(car_data[,3:5],2,min))
data5 = rbind(data5,apply(cl5[,3:5],2,mean))
colnames(data5) = colnames(car_data[,3:5])

data6 = data.frame()
data6 = rbind(data6,apply(car_data[,3:5],2,max))
data6 = rbind(data6,apply(car_data[,3:5],2,min))
data6 = rbind(data6,apply(cl6[,3:5],2,mean))
colnames(data6) = colnames(car_data[,3:5])

library(fmsb)
install.packages("fmsb")

par(mfrow=c(2,3))
radarchart(data1,seg=6,pcol=1,pfcol=topo.colors(6)[1] , plwd=2,plty=1,
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=1,title="Cluster 1")
radarchart(data2,seg=6,pcol=2,pfcol=topo.colors(6)[2] , plwd=2,plty=1,
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=1,title="Cluster 2")
radarchart(data3,seg=6,pcol=3,pfcol=topo.colors(6)[3] , plwd=2,plty=1,
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=1,title="Cluster 3")
radarchart(data4,seg=6,pcol=4,pfcol=topo.colors(6)[4] , plwd=2,plty=1,
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=1,title="Cluster 4")
radarchart(data5,seg=6,pcol=5,pfcol=topo.colors(6)[5] , plwd=2,plty=1,
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=1,title="Cluster 5")
radarchart(data6,seg=6,pcol=6,pfcol=topo.colors(6)[6] , plwd=2,plty=1,
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=1,title="Cluster 6")
old.par = par(mar = c(3, 4, 1, 2))
par(old.par)

par(mfrow=c(2,3),cex.axis=0.7,cex.lab=0.9)
plot(table(cl1[,6]),col=2:9,xlab='Vehicle Type',ylab='Frequency',main='Cluster 1')
plot(table(cl2[,6]),col=2:9,xlab='Vehicle Type',ylab='Frequency',main='Cluster 2')
plot(table(cl3[,6]),col=2:9,xlab='Vehicle Type',ylab='Frequency',main='Cluster 3')
plot(table(cl4[,6]),col=2:9,xlab='Vehicle Type',ylab='Frequency',main='Cluster 4')
plot(table(cl5[,6]),col=2:9,xlab='Vehicle Type',ylab='Frequency',main='Cluster 5')
plot(table(cl6[,6]),col=2:9,xlab='Vehicle Type',ylab='Frequency',main='Cluster 6')

par(mfrow=c(2,3),cex.axis=0.9,cex.lab=0.9)
plot(table(cl1[,7]),col=2:9,xlab='Fuel Type',ylab='Frequency',main='Cluster 1')
plot(table(cl2[,7]),col=2:9,xlab='Fuel Type',ylab='Frequency',main='Cluster 2')
plot(table(cl3[,7]),col=2:9,xlab='Fuel Type',ylab='Frequency',main='Cluster 3')
plot(table(cl4[,7]),col=2:9,xlab='Fuel Type',ylab='Frequency',main='Cluster 4')
plot(table(cl5[,7]),col=2:9,xlab='Fuel Type',ylab='Frequency',main='Cluster 5')
plot(table(cl6[,7]),col=2:9,xlab='Fuel Type',ylab='Frequency',main='Cluster 6')

table(cl1$`Vehicle Type`)

table(cl1$`V`)
table(cl2$`Fuel Type`)
table(cl3$`Fuel Type`)
table(cl4$`Fuel Type`)
table(cl5$`Fuel Type`)
table(cl6$`Fuel Type`)


table(cl5$`Vehicle Type`)
table(cl1$`Vehicle Type`)
table(cl2$`Vehicle Type`)
table(cl3$`Vehicle Type`)
table(cl4$`Vehicle Type`)
table(cl6$`Vehicle Type`)


table(cl1$Transmission)
table(cl2$Transmission)
table(cl3$Transmission)
table(cl4$Transmission)
table(cl5$Transmission)
table(cl6$Transmission)

table(car_data[which(car_data$`Power Steering`=="Yes"),]$cluster)
table(car_data[which(car_data$`Power Steering`=="No"),]$cluster)

table(car_data[which(car_data$Airbag=="Yes"),]$cluster)
table(car_data[which(car_data$Airbag=="No"),]$cluster)

table(car_data[which(car_data$`Power Steering`=="Yes"),]$cluster)
table(car_data[which(car_data$`Power Steering`=="No"),]$cluster)


apply(cl1[,3:5],2,mean)
apply(cl2[,3:5],2,mean)
apply(cl3[,3:5],2,mean)
apply(cl4[,3:5],2,mean)
apply(cl5[,3:5],2,mean)
apply(cl6[,3:5],2,mean)


#eclust(car_data[,-c(1,2,6,7)],FUNcluster = "hclust",hc_metric = "binary",hc_method = 'complete')
#### Comparison of K-means with hierarchical on numerical variables only 
car1 = car_data[,c(3:5)]
car1 = scale(car1,center=T,scale=T)

par(mfrow=c(1,1))
K <- (nrow(car1)-1)*sum(apply(car1,2,var))
for (i in 2:15) K[i] <- sum(kmeans(car1, centers=i)$withinss)
plot(1:15, K, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

### Hierarchical followed by k-means
d = dist(car1)
fit1 = hclust(d,method='complete')
plot(fit1,cex=0.01)
K = nrow(9)
for (i in 2:10){
  cut_avg = cutree(fit1,i)
  k=cluster.stats(d,cut_avg)
  K[i-1] = k$within.cluster.ss
}
plot(2:10,K,type='b',xlab='Number of Clusters',ylab='Within SS')
cut_avg1 = cutree(fit1,7)

centers=aggregate(car1,by=list(cut_avg1),FUN=mean)
centers[,2:4]
km3 = kmeans(car1,centers[,2:4])
fviz_cluster(km3,data=car1,geom="point",pointsize = 1.2,show.clust.cent = T)



#hierarchical suggests a total of 7 clusters, we will choose that as our final K

km1 = kmeans(car1,7)
aggregate(car1,by=list(km1$cluster),FUN=mean)
fviz_cluster(km1,data=car1,geom="point",pointsize = 0.7)

#We are able to get a good clustering result, but we can see there is an outlier which is
#affecting the cluster, let's remedy that 

km2 = kmeans(car1[-c(439,438),],6)
#aggregate(car1,by=list(km1$cluster),FUN=mean)
fviz_cluster(km2,data=car1[-c(439,438),],geom="point",pointsize = 0.7)


### Comparison of clustering techniques

wb=c()
n=c()
asw=c()
wss=c()
ch=c()
d = dist(car1)

k1=cluster.stats(d,km2$cluster,silhouette = F)
k2=cluster.stats(d,km3$cluster,silhouette = F)
k3=cluster.stats(godist,car_data$cluster)
wb =c(wb,k1$wb.ratio)
wb =c(wb,k2$wb.ratio)
wb =c(wb,k3$wb.ratio)

ch =c(ch,k1$ch)
ch =c(ch,k2$ch)
ch =c(ch,k3$ch)

wss =c(wss,k1$within.cluster.ss)
wss =c(wss,k2$within.cluster.ss)
wss =c(wss,k3$within.cluster.ss)

asw =c(asw,k1$avg.silwidth)
asw =c(asw,k2$avg.silwidth)
asw =c(asw,k3$avg.silwidth)
km2$cluster

k1$sindex
?silhouette

dun=c()
dun=c(dun,k1$dunn2)
dun=c(dun,k2$dunn2)
dun=c(dun,k3$dunn2)

comparison =data.frame()
comparison = rbind(comparison,wb)
comparison = rbind(comparison,wss)
comparison = rbind(comparison,ch)
comparison = rbind(comparison,dun)
comparison = rbind(comparison,n)

comparison
