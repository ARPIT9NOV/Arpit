wine <- read.csv("C://R Programme//Video Tutorial assignments//PCA//wine.csv")
View(wine)
data <- wine[-1]
attach(data)
cor(data)
pairs(data)

pcaobj <- princomp(data,cor = TRUE,scores = TRUE,covmat = NULL)
str(pcaobj)

summary(pcaobj)
str(pcaobj)
loadings(pcaobj)
plot(pcaobj)
biplot(pcaobj)
plot(cumsum(pcaobj$sdev*pcaobj$sdev)*100/(sum(pcaobj$sdev*pcaobj$sdev)),type="b")
pcaobj$scores[,1:5]

mydata<-cbind(data,pcaobj$scores[,1:5])
View(mydata)

clus_data<-mydata[,14:18]
norm_clus<-scale(clus_data)
dist1<-dist(norm_clus,method = "euclidean")
fit1<-hclust(dist1,method="complete")
str(fit1)
plot(fit1,hang = -1)
groups <- cutree(fit1,k=5)
membership <- as.matrix(groups)
View(membership)

final <- cbind(membership,wine)
View(final)
View(aggregate(final[,-c(2,14:18)],by=list(membership),FUN=mean))

write.csv(final,file="wine_pca.csv",row.names = F,col.names = F)
getwd()
