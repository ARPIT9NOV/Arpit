claimants <- read.csv("E:\\DOWNLOADS\\Data Science\\Shrinivas Material\\Logistic\\claimants.csv") # Choose the claimants Data set
View(claimants)
attach(claimants)


fit1<-glm(ATTORNEY~CLMSEX+CLMINSUR+SEATBELT+CLMAGE+LOSS,data = claimants,family = "binomial")
summary(fit1)

library(lattice)
library(VIM)
library(magrittr)
library(mice)
library(dplyr)
summary(claimants)
claim <- claimants[-1]
attach(claim)
#convert clamex clminsur,seatbelt into factor      
col <- c("CLMSEX","CLMINSUR","SEATBELT")

claim %<>% mutate_each_(funs(factor(.)),col)
md.pattern(claim)

claim_miss <- aggr(claim,col=c('yellow','red'),numbers=TRUE,sortvars=TRUE,labels=names(claim),cex.axis=.5,cex.numbers=0.5,ylab=c("Proportion of missingness","Missingness Pattern"))

mice_imputes <- mice(claim,m=5,maxit = 40)
mice_imputes$method

mice_imputes$imp$CLMAGE
summary(mice_imputes$imp$CLMAGE)

imputed_data <- complete(mice_imputes,5)

#Plotting and comparing values with xyplot
#xyplot(mice_imputes,CLMINSUR~CLMSEX | .imp, mdc=c(1:2),cex=1.4)

#inspecting strip plot to check if imputed values follow the observed value patterns
#it shows the distribution of individual points
stripplot(mice_imputes,pch=20,cex=1.2)
densityplot(mice_imputes)

model1 <- with(mice_imputes,glm(ATTORNEY~CLMSEX+CLMINSUR+SEATBELT+CLMAGE+LOSS,family ='binomial'))
summary(model1)

pool_model <- pool(model1)
summary(pool_model)

mice_imputes1 <- mice(claim,m=20,maxit = 40)
summary(mice_imputes1)

model2 <- with(mice_imputes1,glm(ATTORNEY~CLMSEX+CLMINSUR+SEATBELT+CLMAGE+LOSS,family ='binomial'))
summary(model2)

pool1_model2 <- pool(model2)
summary(pool1_model2)

model3 <- glm(ATTORNEY~CLMSEX+CLMINSUR+CLMAGE+LOSS,data=imputed_data, family ='binomial')
summary(model3)
# Linear regression technique can not be employed
prob1 <- predict(model3,type="response")
# Logistic Regression 
logit<-glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit)

logit1<-glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit1)

# Odds Ratio
exp(coef(logit1))


# Confusion matrix table 
prob <- predict(model3,type=c("response"),imputed_data)
prob
confusion<-table(prob>0.5,imputed_data$ATTORNEY)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy


# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,imputed_data$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
