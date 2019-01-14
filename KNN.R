data <- read.csv("C:\\R Programme\\KNN\\glass.csv")
View(data)
library(caret)
library(class)
library(gmodels)

table(data$Type)
str(data$Type)
data$Type <- factor(data$Type)
round(prop.table(table(data$Type)) * 100, digits = 1)
summary(data[c("Al","Mg","K")])
str(data)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data1 <- as.data.frame(lapply(data[-10],normalize))
data2 <- cbind(data[10],data1)
View(data2)
typeof(data2)


intrain <- createDataPartition(data2$Type,p=0.75,list = FALSE)
training <- data2[intrain,]
testing <- data2[-intrain,]

training_label <- data2[intrain,1]
testing_label <- data2[-intrain,1]

model <- knn(train = training,test = testing,cl=training_label,k=20)
model
a=table(model,testing[,1])
sum(diag(a))/sum(a)

Res<-c()
model_res<-c()
for (i in 1:13){
  intrain <- createDataPartition(data2$Type,p=0.75,list = FALSE)
  training <- data2[intrain,]
  testing <- data2[-intrain,]
  
  training_label <- data2[intrain,1]
  testing_label <- data2[-intrain,1]
  
  model1 <- knn(train = training,test = testing,cl=training_label,k=20)
  model_res<-c(model_res,model1)
  a<-table(model,testing[,1])
  out<- sum(diag(a))/sum(a)
  Res <- c(Res,out)
}
summary(Res)

Res1<-c()
for (i in 1:13){
  intrain <- createDataPartition(data2$Type,p=0.75,list = FALSE)
  training <- data2[intrain,]
  testing <- data2[-intrain,]
  
  training_label <- data2[intrain,1]
  testing_label <- data2[-intrain,1]
  
  model2 <- knn(train = training,test = testing,cl=training_label,k=15)
  a<-table(model2,testing$Type)
  b <- sum(diag(a))/sum(a)
  Res1 <- c(Res1,b)
}
summary(Res1)
