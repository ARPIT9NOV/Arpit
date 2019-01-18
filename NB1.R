library(e1071)

train <- read.csv("C://R Programme//NB//SalaryData_Train.csv",stringsAsFactors = TRUE)
test <- read.csv("C://R Programme//NB//SalaryData_Test.csv",stringsAsFactors = TRUE)

data <- rbind(train,test)
View(data)

data$Salary <- ifelse(data$Salary==">50k","High","Low")
low_sal<-data[data$Salary=="Low",]
high_sal<-data[data$Salary=='High',]

#Model Build 
library(e1071)

model <- naiveBayes(Salary ~.,data = train)
model

#predict model
predict_model <- predict(model,test)
confusionMatrix(table(predict_model,test$Salary))

library(gmodels)
CrossTable(predict_model, test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
model1 <- naiveBayes(Salary ~.,data = train,laplace = 5)
model1

predict_model1 <- predict(model1,test)
confusionMatrix(table(predict_model1,test$Salary))
CrossTable(predict_model1, test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
