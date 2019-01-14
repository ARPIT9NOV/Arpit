data <- read.csv("C://R Programme//Logistic regression//bank-full.csv")
View(data)
boxplot(data)

boxplot(data$age)
summary(data$age)
bench <- 48+1.5*IQR(data$age) #mean(data$age)
bench
data$age[data$age>bench] <- bench
boxplot(data$age)
hist(data$age)

boxplot(data$balance)
summary(data$balance)
bench <- 1428+1.5*IQR(data$balance)
bench
data$balance[data$balance>bench] <- bench
boxplot(data$balance)
hist(data$balance)
summary(data$balance)
bench <- 72-1.5*IQR(data$balance)
bench
data$balance[data$balance<bench] <- bench
boxplot(data$balance)
hist(data$balance)
summary(data$balance)

boxplot(data$day)

boxplot(data$duration)
summary(data$duration)
bench <- 319+1.5*IQR(data$duration)
bench
data$duration[data$duration>bench] <- bench
boxplot(data$duration)
hist(data$duration)

boxplot(data$campaign)
summary(data$campaign)
bench <- 3+1.5*IQR(data$campaign)
bench
data$campaign[data$campaign>bench] <- bench
boxplot(data$campaign)
hist(data$campaign)

boxplot(data$pdays)
summary(data$pdays)


boxplot(data$previous)
summary(data$previous)
bench <- mean(data$previous)
bench
data$previous[data$previous>bench] <- bench
boxplot(data$previous)
hist(data$previous)
summary(data$previous)


data1 <- as.data.frame(lapply(data,as.numeric))
View(data1)
data1$y <- ifelse(data1$y>=2,1,0)
str(data1)

summary(data1)
cor(data1)

model <- glm(data1$y~.,data=data1,family = 'binomial')
summary(model)
plot(model)

exp(coef(model))

prob <- predict(model,data1,type="response")
summary(model)

confusion<-table(prob>0.5,data1$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
data1[,"prob"] <- prob
data1[,"pred_values"] <- pred_values
data1[,"yes_no"] <- yes_no

View(data1[,c(17,18:20)])

table(data1$y,data1$pred_values)

library(ROCR)
rocrpred<-prediction(prob,data1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
