#Author: Jason

library("apt")
Value1 = log(testdataalgo$PX1)
Value2 = log(testdataalgo$PX2)
Date = testdataalgo$Date

#Calculate the correlationship
cor(Value1,Value2)#0.9306611
########Need to be transform to be Kendall's tau by Copula function

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

#spread
spread = val1 - 0.7427 * val2
msspread = spread - mean(spread)
View(msspread)
plot(c(1:length(msspread)),msspread,type = "l")

#Confirm the arbitrage interval
acf(msspread)#找MA（q）q = 1
pacf(msspread)#找AR（p）p = 2

#Set up AR(2) model
library("TSA")
library("fBasics")
library("fUnitRoots")
library("fGarch")
m1 = arima(msspread,order = c(2,0,0));
#然后对其残差的平方序列ARCH效应检验，通过做Ljung-Box 检验进行判定
re = m1$residuals
Box.test(re^2,lag = 5,type = 'Ljung');
Box.test(re^2,lag = 10,type = 'Ljung');
Box.test(re^2,lag = 15,type = 'Ljung');
#当阶数为10 是都有非常小的p 值，说明序列存在明显的ARCH 效应
#再对残差做Mcleod-Li检验，由图可见所有的p 值都在0.05以下，说明有显著的ARCH效应
McLeod.Li.test(m1,re,gof.lag = 30,plot = TRUE)

#Use GARCH model to fit the 
m2 = garchFit(~garch(1,1),data = msspread,trace = F)
m2
summary(m2)

#student-distribution
m3 = garchFit(~garch(1,1),data = msspread,trace = F, cond.dist = c("std"))
m3
summary(m3)

#sstd
m4 = garchFit(~garch(1,1),data = msspread,trace = F, cond.dist = c("sstd"))
m4
summary(m4)
tsdiag(m4) ##生成残差标准差、残差ACF、残差各阶t 检验对应的P 值
#但是为什么用不鸟。。。。。。妈个鸡
##感觉msspread的残差还是更加服从一个有偏的t分布
qqnorm(msspread,xlab = "Q-Q plot for normal dsn")
qqline(msspread)#效果太差

qqplot(qsstd(ppoints(250), nu = 4.832,xi = 0.995), msspread, xlab = "Q-Q plot for skew-t dsn")
qqline(msspread)

qqplot(qt(ppoints(250), df = 4.832), msspread, xlab = "Q-Q plot for t dsn")
qqline(msspread)
###用QQ plot 发现的确是一个t distibution 而且用t分布比有偏的t分布效果更好
#使用model 3