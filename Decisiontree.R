library(C50)

data <- read.csv("C://R Programme//Decisiontree//Fraud_check.csv")
a <-factor(ifelse(data$Taxable.Income<=30000,"Risky","Good"))
table(a)
View(data)
data1 <-cbind(a,data)
data1 <- as.data.frame(data1)
attach(data1)

sam <- sample(nrow(data1),size=0.75*nrow(data1),replace = F)
training <- data1[sam,]
testing <- data1[-sam,]

model <-C5.0(a~.,data=training)
pred <- predict.C5.0(model,testing[,-1])
b <- table(testing$a,pred)

sum(diag(b))/sum(b)
windows()
plot(model)
