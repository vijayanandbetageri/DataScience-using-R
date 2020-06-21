#Logistic 
bank.full <- read.csv(file.choose())# Choose the claimants Data set
View(bank.full)
data=bank.full[,-1]
data
bank.full <- na.omit(bank.full)
View(bank.full)
model <- glm(y~.,data=bank.full[,-1],family = "binomial")

# Confusion matrix table 
prob <- predict(model,type=c("response"),bank.full)
prob

confusion<-table(prob>0.5,bank.full$y)
confusion

confusion# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62

##
pred_values <- NULL
yes_no <- NULL
for (i in 1:45211){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

bank.full[,"prob"] <- prob
bank.full[,"pred_values"] <- pred_values
bank.full[,"yes_no"] <- yes_no
View(bank.full)

View(bank.full[,c(2,8,9,10)])

# Accuracy 
acc <- table(bank.full$y,pred_values)
acc
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy # 70.62

# ROC Curve 
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,bank.full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

