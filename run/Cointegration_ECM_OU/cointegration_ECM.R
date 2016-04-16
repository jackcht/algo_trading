# Author: Jason

library("apt")
Value1 = testdataalgo$PX1
Value2 = testdataalgo$PX2
Date = testdataalgo$Date
plot(Date,Value1)
#Calculate the correlationship
cor(Value1,Value2)#0.9223518
##Need to be transform to be Kendall's tau by Copula function

#Cointegration test
#install.packages("fUnitRoots")
library(fUnitRoots);
adfTest(Value1)
adfTest(Value2)

###I = 1, so they are I(1)
val1 = diff(Value1)
val2 = diff(Value2)

adfTest(val1)
adfTest(val2)
###Now the two series are stationary

#ECM model
reg<-lm(Value1~Value2)
summary(reg)
#install.packages("lmtest")
library("lmtest")
dw<-dwtest(reg)#Durbin-Wastion test
dw

#install.packages("urca")
library(urca)
error<-residuals(reg)
adfTest(error)

error.lagged <- diff(error)
ecm.reg1<-lm(val1~error.lagged+val2)
summary(ecm.reg1)
dwtest(ecm.reg1)
#get the amount of trading
