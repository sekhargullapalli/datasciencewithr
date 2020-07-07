#Logistic regression in R
#case: survival prediction - Titanic
#Based on https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

library(dplyr)

#--------Data-----------
remove(list = ls())
rawdata = read.csv("./data/titanic/train.csv",header = TRUE, na.strings = c(""), stringsAsFactors = TRUE)
colnames(rawdata)

#---------Data processing----------------------
library(Amelia)
missmap(rawdata)
data <- subset(rawdata,select=c(2,3,5,6,7,8,10,12))
missmap(data)
#setting missing age to mean
data$Age[is.na(data$Age)]<-mean(data$Age,na.rm =TRUE)
is.factor(data$Sex)
is.factor(data$Embarked)
contrasts(data$Sex)
contrasts(data$Embarked)
#removing rows with missing embarked field, also possible to replace by mode of the column
data<-data[!is.na(data$Embarked),]
rownames(data) <- NULL

#------------Model fitting---------------------------
trainset = data[1:800,]
testset =data[801:nrow(data),]

model <- glm(Survived~.,family = binomial(link = 'logit'),data = trainset)
summary(model)

anova(model, test="Chisq")

#-----------Predict---------------------------

fit <- predict(model,newdata = subset(testset,select = 2:8),type = 'response')
fit<-ifelse(fit>0.5,1,0)
misclassificationerr = mean(fit != testset$Survived)
print(paste("Accuracy",1-misclassificationerr))

#----------------ROC(receiver operating characteristic)------------------------
library(ROCR)

plot(prf, xlab="FPR or (1-specificity)", ylab="TPR or sensitivity")
pr<-prediction(fit,testset$Survived)
prf <- performance(pr,measure = "tpr", x.measure = "fpr")
graphics.off()
#plot(prf)
plot(prf, xlab="FPR or (1-specificity)", ylab="TPR or sensitivity")

#area under curve
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc








