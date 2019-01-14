library(plyr)
library(ggplot2)
x <- runif(50)
x
y <- runif(50)
y
data <- cbind(x,y)
data
boxplot(data)
plot(data)
plot(data,type="n")
text(data,rownames(data))
km <- kmeans(data,4) #kmeans clustering
str(km)

install.packages("animation")
library(animation)

km1 <- kmeans.ani(data,4)
str(km1)
km$cluster

km$centers


##  K means clustering crime data set
data<-read.csv("C://R Programme//Video Tutorial assignments//9 Clusterproblem//crime_data.csv")
mydata <- data[ ,2:5]
normalized_data <- scale(mydata[ ,2:4])
fit <- kmeans(normalized_data,5)
str(fit)
final2 <- data.frame(mydata,fit$cluster)
final2
final3 <- final2[ ,c(ncol(final2),1:ncol(final2)-1)]

aggregate(mydata[,2:4],by=list(fit$cluster), FUN =mean)
library(kselection)
k <- kselection(mydata[,-5],parallel=TRUE,k_threshold=0.9,max_centers=12)
library(doParallel)
?doParallel
registerDoParallel(cores=4)
k <- kselection(mydata[,-5],parallel=TRUE,k_threshold=0.9,max_centers=12)
k


# elbow curve & k value
wss=(nrow(normalized_data)-1)*sum(apply(normalized_data,2,var)) # determined number of column
for(i in 2:8) wss[i]=sum(kmeans(normalized_data,centers=i)$withinss)
plot(1:8,wss,type="b",xlab="Number of Clusters",ylab="Within group sum of squares")
title(sub="K-means Clusterin Scree plot")

## Kmeans clustering 
data <- read.csv("C://R Programme//Video Tutorial assignments//9 Clusterproblem//crime_data.csv")
View(data)
data <- data[-1]
norm_data <- scale(data)
d <- dist(norm_data,method = "euclidean")
fit <- hclust(d,method = "centroid")
str(fit)
plot(fit,hang = -1)
groups <- cutree(fit,k=4)
rect.hclust(fit,k=4,border='red')
membership <- as.matrix(groups)
final <- data.frame(norm_data,membership)
final
final <- final[,c(ncol(final),1:ncol(final)-1)]
final
write.csv(final,file = "crime_dataclust.csv")

### EAstern airlines dataset clustering

data <- EastWestAirlines...Copy[-1]
normalized_data <- scale(data)
data_z <- normalized_data
sam <- data[sample(nrow(data_z),100),]
d <- dist(sam,method="euclidean")
fit <- hclust(d,method="complete")
str(fit)
plot(fit,hang=-1)
groups <- cutree(fit,k=5)
rect.hclust(fit,k=5,border="red")
membership <- as.matrix(groups)
final <- data.frame(sam,membership)
final1 <- final[ ,c(ncol(final),1:ncol(final)-1)]
write.csv(final1,file="final1.csv")

## Eastwestern line Kmeans
data <- read.csv("C://R Programme//Video Tutorial assignments//9 Clusterproblem//EastWestAirlines - Copy.csv")
View(data)
norm_data <- scale(data[-1])
fit <- kmeans(norm_data,5)
str(fit)
final <- data.frame(norm_data,fit$cluster)
final
aggregate(norm_data,by=list(fit$cluster), FUN = mean)
library(kselection)
k <- kselection(norm_data,parallel=TRUE,k_threshold=0.9,max_centers=12)
library(doParallel)
?doParallel
registerDoParallel(cores=4)
k <- kselection(norm_data,parallel=TRUE,k_threshold=0.9,max_centers=12)
k
# elbow curve & k value
wss=(nrow(norm_data)-1)*sum(apply(norm_data,2,var)) # determined number of column
for(i in 2:8) wss[i]=sum(kmeans(norm_data,centers=i)$withinss)
plot(1:8,wss,type="b",xlab="Number of Clusters",ylab="Within group sum of squares")
title(sub="K-means Clusterin Scree plot")
