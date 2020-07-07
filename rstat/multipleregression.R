#Muliple regression in R

library(dplyr)

#--------Data-----------
remove(list = ls())
df <- read.delim("./data/multipleregression1.csv", sep=",", comment.char = '#')
df <- na.omit(df)
df <- data.frame(lapply(df, as.numeric))
df

#------------Regression--------------------
model <- lm(y~x1+x2+x3,df)
summary(model)
coef(model)

#------------prediction--------------------
ypred = predict(model,df)
df <- cbind(df,ypred)
glm

#---------visualization------------------
#using euclid distance for x to represent in a single plot
dfv <- df%>%select(y,x1,x2,x3,ypred) %>% mutate(xeuclid = (x1^2)+(x2^2)+(x3^2)) %>% select(y,xeuclid,ypred)
plot(dfv$xeuclid,dfv$y,col="blue", pch=19,main = "Multiple regression", xlab = "euclid representation of x1,x1,x3", ylab = "dependent variable")
points(dfv$xeuclid,dfv$ypred,col="red",pch=5)
txt = paste(round(coef(model),5),collapse = ", ")
txt=paste("fit: ",txt,"  r-squared", round(summary(model)$r.squared,5), sep = " ")
text(min(dfv$xeuclid),max(dfv$y),txt, cex = 0.8, pos=4, offset = 1, adj = c(0,1))



