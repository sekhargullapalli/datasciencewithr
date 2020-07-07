#Linear regression in R

library(dplyr)

#--------Data-----------
remove(list = ls())
df <- read.delim("./data/linearregression1.csv", sep=",", comment.char = '#')
df <- na.omit(df)

#------------Regression--------------------
model <- lm(Y~X,df)
summary(model)

#-------------Predicting---------------------
Ypred=predict(model,df%>%select(X))
df <- cbind(df,Ypred)
df <- df%>%mutate(AbsErr=abs(Y-Ypred))
df <- df%>%mutate(AbsPercentErr=abs(AbsErr/Y*100))
View(df)

#---------------Plotting---------------------------

plot(df$X,df$Y,col="blue", pch=19,main = "Linear regression", xlab = "grain size mm", ylab = "beach slope degrees")

abline(model, col="red", lty=1, lwd=2)

points(df$X,df$Ypred,col="red",pch=5)

coeff <- paste("fit: intercept:", round(model$coefficients[1],4),
               ",slope:",round(model$coefficients[2],4),sep = " ")
rms <- paste("r.squared:",round(summary(model)$r.squared,4),sep = " ")
txt <- paste(coeff, rms, sep = "  ")
text(min(df$X),max(df$Y),txt, cex = 0.8, pos=4, offset = 1, adj = c(0,1))
