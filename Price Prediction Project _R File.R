library(readxl)
library(tidyverse)
library(forecast)
library(dplyr)

#Import the excel table into the dataframe
df <- read_xlsx("D:/CPS Analytics/Quarters/Q3/Intro to Enterprise/Week 3/Honeywell.xlsx", sheet = "HON-1", range = cell_cols("B:C"))
values <- df[, -1]  #(excluded first column i.e date in real dataset)
TS <- ts(values,start=c(2017,10),end=c(2018,4),frequency=365)

plot(TS, main = "Time series Plot")
#Part 1 - Simple Exponential Smoothing

forecast1<- ses(TS, h=3, alpha=0.15, initial="simple")
summary(forecast1)


forecast2<- ses(TS, h=3, alpha=0.35, initial="simple")
summary(forecast2)

forecast3<- ses(TS, h=3, alpha=0.55, initial="simple")
summary(forecast3)

forecast4<- ses(TS, h=3, alpha=0.75, initial="simple")
summary(forecast4)

#Part 2 - Adjusted Exponential Smoothing forecast
fit<- holt(TS, h=3, alpha=0.75,beta = 0.15,initial="simple")
summary(fit)

fit2<- holt(TS, h=3, alpha=0.75,beta = 0.25, initial="simple")
summary(fit2)

fit3<- holt(TS, h=3, alpha=0.75,beta = 0.45, initial="simple")
summary(fit3)

fit4<-  holt(TS, h=3, alpha=0.75,beta = 0.85, initial="simple")
summary(fit4)

#Part 3 - Simple Regression Analysis
df1 <- read_xlsx("D:/CPS Analytics/Quarters/Q3/Intro to Enterprise/Week 3/Honeywell.xlsx", sheet = "HON-1", range = cell_cols("A:C"))

regmodel<- lm(Close ~ Period, data = df1)
summary(regmodel)


#Coefficient of correlation
cor(df1$Close,df1$Period)

#Regression model for prediction 

price = 1.504e+02+(8.926e-03*127) # using Y = intecept + slope * X prediction price is 151.5336

#Mean Squared error
mean((summary(regmodel)$residual)^2)

# Histogram of Residuals
Res = resid(regmodel)
hist(Res,main="Histogram of Residuals",col="orange",xlab = "Residuals")

# Normal Probability plot of residuals
qqnorm(Res, main =" Normal Probability Plot of Residuals",col="blue") 
qqline(Res,col="red",lwd = 2)

#Chi squared test for Normality
summary(Res)
sd(Res)
X<- rnorm(124,0,5)


#calculating Range of distribution
Min1 <- min(Res)-0.5
Max1 <- max(Res)+0.5
Range <- Max1 - Min1


#considering bins 8
Width <- Range/8
Breaks <- seq(Min1, Max1, Width)
Res.cut<-cut(Res,breaks=Breaks)
table(Res.cut)

x<-rnorm(124,0,5)
x.cut<-cut(x,breaks=Breaks)

#Calculating bin wise frequency for observed values ( Residuals)
obs.freq<-vector() 
for(i in 1:8) obs.freq[i]<- table(Res.cut)[[i]]


#Calculating bin wise frequency for expected values (randomly generated normal numbers)
exp.freq<-vector() 
for(i in 1:8) exp.freq[i]<- table(x.cut)[[i]]

#Chi Squared Test
DF <- 8-1
test.statistic<-sum(((obs.freq-exp.freq)^2)/exp.freq) 
Pvalue<-1-pchisq(test.statistic,DF) 
Pvalue


# Scatterplot Residual V/s period

plot(df1$Period, Res,ylab="Residuals", xlab="Period", main = "Residual v/s Time Period", col = "brown",pch = 16) 

#Scatterplot Residual V/s Closing price
plot(df1$Close, Res,ylab="Residuals", xlab="Closing Price",main = "Residual v/s Closing price",col ="green",pch = 16) 

