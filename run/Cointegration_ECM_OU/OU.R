# Author: Jason

library("apt")
y = log(testdataalgo$PX1)
x = log(testdataalgo$PX2)
Date = testdataalgo$Date

############################################################
#a loop to get a matrix for correlation of stock pairs.
############################################################

#Calculate the correlationship
cor(x,y)#0.9306611
########Need to be transform to be Kendall's tau by Copula function

#Cointegration test
#install.packages("fUnitRoots")
library(fUnitRoots);
adfTest(x)
adfTest(y)

###I = 1, so they are I(1)
diff.x = diff(x)
diff.y = diff(y)

adfTest(diff.x)
adfTest(diff.y)
###Now the two series are stationary

#ECM model
lm1<-lm(y~x)
summary(lm1)
#install.packages("lmtest")
library("lmtest")
dw<-dwtest(lm1)#Durbin-Wastion test
dw

#install.packages("urca")
library(urca)
lm1.error<-residuals(lm1)
adfTest(lm1.error)
#####################################################
###############If use ADL(1,1) Model#################
#x.lag.1 = lag(x,k = 1);
#y.lag.1 = lag(y,k = 1);
#lm2 <- lm(y ~ x + lag(x) + lag(y))
#summary(lm2)
#####################################################

####################ECM Model#######################
lm1.error.diff <- diff(lm1.error)
lm2.ecm<-lm(diff.y~lm1.error.diff + diff.x)
summary(lm2.ecm)
dwtest(lm2.ecm)
#get the amount of trading

#spread
spread = diff.y - 0.7427* diff.x;
msspread = spread - mean(spread)
View(msspread)
plot(c(1:length(msspread)),msspread,type = "l")

############################
######GARCH Model###########
############################

#Confirm the arbitrage interval
acf(msspread)#找MA（q）q = 2
pacf(msspread)#找AR（p）截尾 p = 2

#Set up AR(2) model
library("TSA")
library("fBasics")
library("fUnitRoots")
library("fGarch")
arima1 = arima(msspread,order = c(2,0,0));
#然后对其残差的平方序列ARCH效应检验，通过做Ljung-Box 检验进行判定
re = arima1$residuals
Box.test(re^2,lag = 5,type = 'Ljung');
Box.test(re^2,lag = 10,type = 'Ljung');
Box.test(re^2,lag = 15,type = 'Ljung');
#当阶数为10 是都有非常小的p 值，说明序列存在明显的ARCH 效应
#再对残差做Mcleod-Li检验，由图可见所有的p 值都在0.05以下，说明有显著的ARCH效应
McLeod.Li.test(arima1,re,gof.lag = 30,plot = TRUE)

#观察Ljung-Box test的结果：p要越大越好，这样就反应了残差已经消除了ARCH效应
#Use GARCH model to fit the 
garch.norm = garchFit(~garch(1,2),data = msspread,trace = F)
garch.norm
summary(garch.norm)
garch.norm.re <- residuals(garch.norm)

#student-distribution
garch.t <- garchFit(~garch(1,1),data = msspread,trace = F, cond.dist = c("std"))
garch.t
summary(garch.t)
garch.t.re <- residuals(garch.t)

#sstd
garch.skewt = garchFit(~garch(1,2),data = msspread,trace = F, cond.dist = c("sstd"))
garch.skewt
summary(garch.skewt)
garch.skewt.re <- residuals(garch.skewt)

#tsdiag(garch.skewt) ##生成残差标准差、残差ACF、残差各阶t 检验对应的P 值
#但是为什么用不鸟。。。。。。妈个鸡
##感觉msspread的残差还是更加服从一个有偏的t分布
qqnorm(garch.norm.re,xlab = "Q-Q plot for normal dsn")
qqline(garch.norm.re)

qqplot(qsstd(ppoints(250), nu = 4.831,xi = 0.9994), garch.t.re, xlab = "Q-Q plot for skew-t dsn")
qqline(garch.skewt.re)

qqplot(qt(ppoints(250), df = 4.831), garch.t.re, xlab = "Q-Q plot for t dsn")
qqline(garch.t.re)# perfect match

############################
######O-U Process###########
############################
Xt = msspread;
oulm = arima(Xt,order = c(1,0,0))
oulm.residual = oulm$residuals;
a = 1; b = 1-0.07505941;
dt = 1/252;
theta = -log(abs(b))/dt;
sigma = sqrt(var(oulm.residual*2*theta)/(1-b^2))
sigma.x = sqrt(var(oulm.residual)/(1-b^2))

#The best trading signal
c = 0.5/100;
a.star = -c/4 - (c^2 * a)/(4*(c^3*a^3+24*c*a^2*sigma^2-4*sqrt(3*c^4*a^5*sigma^2+36*c^2*a^4*sigma^4)))^(1/3)
m = mean(spread);
delta = (m-a.star)/sd(spread);
#Also we should trying different delta = c(0.01:100) to test and get the best strats.