normalize <- function(x){
  (x-min(x))/(max(x)-min(x))
}
data <- read.csv("C://R Programme//Neural Network Ass//50_Startups.csv")
data <- data[-4]
data1 <- as.data.frame(lapply(data,normalize))

library(neuralnet)
acc <- c()
for(i in 1:11){
sam <- sample(nrow(data1),size = 0.75*nrow(data1),replace = F)
train <- data1[sam,]
test <- data1[-sam,]

profit_model <- neuralnet(formula=Profit~R.D.Spend+Administration+Marketing.Spend,
                          data = train,hidden=3)
plot(profit_model)
model_result <- compute(profit_model, test[1:3])
predicted_profit <- model_result$net.result
a <- cor(predicted_profit,test$Profit)
acc <- c(acc,a)
}
summary(acc)
