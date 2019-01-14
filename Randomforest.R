library(randomForest)

data <- read.csv("C://R Programme//Decisiontree//Fraud_check.csv")

fraudcheck <-ifelse(data$Taxable.Income<=30000,"Risky","Good")
table(fraudcheck)
View(data)

data1 <-cbind(data,fraudcheck)
head(data1)
round(prop.table(table(data1$fraudcheck)*100))

sam <- sample(nrow(data1),size = 0.75*nrow(data1),replace = F)
train <- data1[sam,]
test <- data1[-sam,]

model <- randomForest(train$fraudcheck~.,data=train, na.action =na.roughfix,importance=TRUE)
summary(model)
predict_model <- predict(model,test[-7])

result <- table(predict_model,test[,7])
windows()
plot(model)
