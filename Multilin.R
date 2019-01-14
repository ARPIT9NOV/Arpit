## Comp Data Sales Prediction

data <- read.csv("C://R Programme//Multiple linear Assignments//Computer_Data.csv")
View(data)
data <- data[,-1]

#1st method convert into dummy
#library(plyr)
#data$cd<- revalue(data$cd,c("yes"=1))
#data$cd <- revalue(data$cd,c("no"=0)) 
#2nd method convert into dummy
boxplot(data$price)
summary(data$price)
bench <- 2595+1.5*IQR(data$price)
bench
data$price[data$price>bench] <- bench
boxplot(data$price)
summary(data$price)
hist(data$price)

boxplot(data)
boxplot(data$speed)

boxplot(data$hd)
summary(data$hd)
bench1 <- 528+1.5*IQR(data$hd)
bench1
data$hd[data$hd>bench1] <- bench1
boxplot(data$hd)
summary(data$hd)
hist(data$hd)

boxplot(data$ram)
summary(data$ram)
bench2 <- 8+1.5*IQR(data$ram)
bench2
data$ram[data$ram>bench2] <- bench2
boxplot(data$ram)
summary(data$ram)
hist(data$ram)

boxplot(data$screen)
summary(data$screen)
bench3 <- 15+1.5*IQR(data$screen)
bench3
data$screen[data$screen>bench3] <- bench3
boxplot(data$screen)
summary(data$screen)
hist(data$screen)

data$cd <- as.numeric(as.factor(data$cd))
boxplot(data$cd)

data$multi <- as.numeric(as.factor(data$multi))
boxplot(data$multi)


data$premium <- as.numeric(as.factor(data$premium))
boxplot(data$premium)

boxplot(data$ads)
boxplot(data$trend)

attach(data)
pairs(data)
cor(data)
summary(data)

model <- lm(price~.,data=data)
summary(model) #0.782
