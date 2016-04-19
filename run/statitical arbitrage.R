###跨品种统计套利

setwd("C:/Rise's Space/中大期货实习/统计套利")

##读入数据
data<-read.csv("Futures.csv",stringsAsFactor=F)

##取日期和期货合约代码
date<-data[-(1:2),1]
code<-colnames(data)[seq(2,ncol(data)-1,7)]

##将数据numeric化
data<-data[-(1:2),-1]
data<-apply(data,2,as.numeric)

##取成交量、收盘价、开盘价
volume<-data[,seq(5,ncol(data),7)]
volume<-volume>0
rownames(volume)<-date
colnames(volume)<-code
closeprice<-data[,seq(4,length(colnames(data))-1,7)]
openprice<-data[,seq(1,length(colnames(data))-1,7)]
rownames(closeprice)<-date
colnames(closeprice)<-code
rownames(openprice)<-date
colnames(openprice)<-code

##将成交量为0时的收盘价、开盘价变为0
closeprice<-closeprice*volume
openprice<-openprice*volume

##将收盘价、开盘价中的0变成NA
source("zerotoNA.R")
closeprice<-zerotoNA(closeprice)
openprice<-zerotoNA(openprice)

##取样本内的数据(5y64%) 
ISdate<-date[which(date=="2009/8/20"):which(date=="2012/10/31")]
ISvolume<-volume[ISdate,]
IScloseprice<-closeprice[ISdate,]
ISopenprice<-openprice[ISdate,]

##剔除样本期内有效数据不足的期货合约
period<-apply(ISvolume,2,sum)
validcode<-names(period[which(period>=100)])
IScloseprice<-IScloseprice[,validcode]
ISopenprice<-ISopenprice[,validcode]

##计算两两期货合约间的相关系数
corfutures<-cor(IScloseprice,use="pairwise.complete.obs")
corfutures[upper.tri(corfutures)]<-0
highcorindex<-which(corfutures>0.8 & corfutures!=1)
pair1index<-highcorindex%%nrow(corfutures)
pair1index[which(pair1index==0)]<-length(validcode)
pair1<-validcode[pair1index]
pair2index<-highcorindex%/%nrow(corfutures)+(highcorindex%%nrow(corfutures)!=0)
pair2<-validcode[pair2index]

##相关性检验
for(i in 1:length(pair1)){
  if(cor.test(IScloseprice[,pair1[i]],IScloseprice[,pair2[i]])$p.value>=0.05){
    pair1<-pair1[-i]
    pair2<-pair2[-i]
  }
}

##协整关系检验
library("tseries", lib.loc="~/R/win-library/3.1")
col1<-c() #pair1
col2<-c() #pair2
col3<-c() #intercept
col4<-c() #b
for(i in 1:length(pair1)){
  validpair1data<-IScloseprice[,pair1[i]][!is.na(IScloseprice[,pair1[i]])]
  validpair2data<-IScloseprice[,pair2[i]][!is.na(IScloseprice[,pair2[i]])]
  dvalidpair1data<-diff(validpair1data)
  dvalidpair2data<-diff(validpair2data)
  if(adf.test(validpair1data)$p.value>=0.05 & adf.test(validpair2data)$p.value>=0.05 &
     adf.test(dvalidpair1data)$p.value<0.05 & adf.test(dvalidpair1data)$p.value<0.05){
    lmresult<-lm(IScloseprice[,pair1[i]]~IScloseprice[,pair2[i]])
    lmcoef<-lmresult$coef
    intercept<-lmcoef[1]
    b<-lmcoef[2]
    residual<-residuals(lmresult)
    if(adf.test(residual)$p.value<0.05){
      col1<-c(col1,pair1[i])
      col2<-c(col2,pair2[i])
      col3<-c(col3,intercept)
      col4<-c(col4,b)
    }
  }
}
cointegrationmodel<-cbind(col1,col2,col3,col4)
colnames(cointegrationmodel)<-c("pair1","pair2","intercept","b")

##
annualisedreturn<-c()
maxdrawdown<-c()
annualisedvolatility<-c()
sharperatio<-c()
tradenumber<-c()
winrate<-c()
averagetradeprofit<-c()

for(k in 1:nrow(cointegrationmodel))
{
## 协整模型
#k<-1
p1<-cointegrationmodel[k,1]
p2<-cointegrationmodel[k,2]
beta<-as.numeric(cointegrationmodel[k,4])
c<-as.numeric(cointegrationmodel[k,3])
spread<-IScloseprice[,p1]-(beta*IScloseprice[,p2]+c)
msspread<-spread-mean(spread,na.rm=TRUE)
sdspread<-sd(spread,na.rm=TRUE)
plot(msspread,type="l")

# ##误差修正模型
# k<-1
# p1<-cointegrationmodel[k,1]
# p2<-cointegrationmodel[k,2]
# y<-logIScloseprice[,p1][-1]
# y1<-logIScloseprice[,p1][-(length(y)+1)]
# x<-logIScloseprice[,p2][-1]
# x1<-logIScloseprice[,p2][-(length(x)+1)]
# vec<-lm(y~x1+x+y1)
# beta<-(vec$coef[2]+vec$coef[3])/(1-vec$coef[4])
# c<-vec$coef[1]/(1-vec$coef[4])
# spread<-IScloseprice[,p1]-(beta*IScloseprice[,p2]+c)
# msspread<-spread-mean(spread,na.rm=TRUE)
# plot(msspread,type="l")
# sdspread<-sd(spread,na.rm=TRUE)

##策略参数设置
open<-0.75 #开仓信号参数
close<-0.05 #平仓信号参数
stoploss<-2 #止损信号参数
openupline<-open*sdspread #开仓信号上线
stoplossupline<-stoploss*sdspread #止损信号上线
closeupline<-close*sdspread #平仓信号上线
closedownline<-(-close*sdspread) #平仓信号下线
opendownline<-(-open*sdspread) #开仓信号下线
stoplossdownline<-(-stoploss*sdspread) #止损信号下线
principal<-500000 #初始资金
marginrate<-0.10 #保证金率
openrate<-0.70 #开仓比率
commission<-0.0008 #佣金
impactcost<-0.001 #冲击成本
tradecost<-commission+impactcost #交易成本
signal<-msspread #交易信号：去中心化后的残差
position<-matrix(0,length(ISdate),2) #合约对持仓量
netcapital<-matrix(0,length(ISdate),1) #账户净值（每日结算后）
margin<-matrix(0,length(ISdate),3) #保证金
transaction<-matrix(0,length(ISdate),1) #交易明细
tradeprofit<-matrix(0,length(ISdate),1) #每笔交易盈利
rownames(position)<-ISdate
colnames(position)<-c(p1,p2)
rownames(netcapital)<-ISdate
colnames(netcapital)<-"netcapital"
rownames(margin)<-ISdate
colnames(margin)<-c(p1,p2,"sum")
rownames(transaction)<-ISdate
colnames(transaction)<-"transaction detail"
rownames(tradeprofit)<-ISdate
colnames(tradeprofit)<-"tradeprofit"
netcapital[1,]<-principal

##样本内策略回测
for(i in 2:length(ISdate)){
 
  #前一日状态的延续
  position[i,]<-position[i-1,]
  netcapital[i]<-netcapital[i-1]
  margin[i,]<-margin[i-1,]
  
  #今日可交易的条件
if(!is.na(IScloseprice[i-1,p1]) & !is.na(IScloseprice[i-1,p2]) &
      !is.na(ISopenprice[i,p1]) & !is.na(ISopenprice[i,p2]) &
      !is.na(IScloseprice[i,p1]) & !is.na(IScloseprice[i,p2]) &
      !is.na(signal[i-1]) &
      netcapital[i]>0){
  
  #最后一个交易日平仓(若仍有仓位)
  if(i==length(ISdate)){
    netcapital[i]<-netcapital[i]+position[i,1]*(ISopenprice[i,p1]-IScloseprice[i-1,p1])*(1-tradecost)+position[i,2]*(ISopenprice[i,p2]-IScloseprice[i-1,p2])*(1-tradecost)
    if(position[i,1]>0){
      transaction[i]<-paste("sell",paste(p1,"x",position[i,1],"@",ISopenprice[i,p1]),
                            "buy",paste(p2,"x",-position[i,2],"@",ISopenprice[i,p2]))
      tradeprofit[i]<-(netcapital[i]-opennetcapital)/opennetcapital
    } 
    else if(position[i,1]<0){
      transaction[i]<-paste("buy",paste(p1,"x",-position[i,1],"@",ISopenprice[i,p1]),
                            "sell",paste(p2,"x",position[i,2],"@",ISopenprice[i,p2]))
      tradeprofit[i]<-(netcapital[i]-opennetcapital)/opennetcapital
    }
    position[i,1]<-0
    position[i,2]<-0
    margin[i,1]<-0
    margin[i,2]<-0
  }
  
  #有合约A空头
  else if(position[i,1]<0){
    
    #止损 or 止盈
    if(signal[i-1]>=stoplossupline | signal[i-1]<=closeupline){
      netcapital[i]<-netcapital[i]+position[i,1]*(ISopenprice[i,p1]-IScloseprice[i-1,p1])*(1-tradecost)+position[i,2]*(ISopenprice[i,p2]-IScloseprice[i-1,p2])*(1-tradecost)
      transaction[i]<-paste("buy",paste(p1,"x",-position[i,1],"@",ISopenprice[i,p1]),
                             "buy",paste(p2,"x",position[i,2],"@",ISopenprice[i,p2]))
      tradeprofit[i]<-(netcapital[i]-opennetcapital)/opennetcapital
      position[i,1]<-0
      position[i,2]<-0
      margin[i,1]<-0
      margin[i,2]<-0
    }
  }
  
  #有合约A多头
  else if(position[i,1]>0){
    
    #止损 or 止盈
    if(signal[i-1]<=stoplossdownline | signal[i-1]>=closedownline){
      netcapital[i]<-netcapital[i]+position[i,1]*(ISopenprice[i,p1]-IScloseprice[i-1,p1])*(1-tradecost)+position[i,2]*(ISopenprice[i,p2]-IScloseprice[i-1,p2])*(1-tradecost)
      transaction[i]<-paste("sell",paste(p1,"x",position[i,1],"@",ISopenprice[i,p1]),
                             "buy",paste(p2,"x",-position[i,2],"@",ISopenprice[i,p2]))
      tradeprofit[i]<-(netcapital[i]-opennetcapital)/opennetcapital
      position[i,1]<-0
      position[i,2]<-0
      margin[i,1]<-0
      margin[i,2]<-0
    }
  }
  
  #无合约头寸，根据交易信号决定是否开仓
  else if(position[i,1]==0){
    
    #多A
    if(signal[i-1]<=opendownline & signal[i-1]>stoplossdownline){
      position[i,1]<-round((min(principal,netcapital[i])*openrate/(ISopenprice[i,p1]*(1+tradecost)*marginrate))/(1+beta))
      position[i,2]<-round(-position[i,1]*beta)
      margin[i,1]<-position[i,1]*ISopenprice[i,p1]*marginrate
      margin[i,2]<-(-position[i,2])*ISopenprice[i,p2]*marginrate
      netcapital[i]<-netcapital[i]-margin[i,1]/marginrate*tradecost-margin[i,2]/marginrate*tradecost
      transaction[i]<-paste("buy",paste(p1,"x",position[i,1],"@",ISopenprice[i,p1]),
                             "sell",paste(p2,"x",-position[i,2],"@",ISopenprice[i,p2]))
      opennetcapital<-netcapital[i] #开仓后的净资金
    }
    
    #空A
    else if(signal[i-1]>=openupline & signal[i-1]<stoplossupline){
      position[i,1]<-(-round(min(principal,netcapital[i])*openrate/(ISopenprice[i,p1]*(1+tradecost)*marginrate))/(1+beta))          
      position[i,2]<-round(-position[i,1]*beta)
      margin[i,1]<-position[i,1]*ISopenprice[i,p1]*marginrate
      margin[i,2]<-(-position[i,2])*ISopenprice[i,p2]*marginrate
      netcapital[i]<-netcapital[i]-margin[i,1]/marginrate*tradecost-margin[i,2]/marginrate*tradecost
      transaction[i]<-paste("sell",paste(p1,"x",-position[i,1],"@",ISopenprice[i,p1]),
                             "buy",paste(p2,"x",position[i,2],"@",ISopenprice[i,p2]))
      opennetcapital<-netcapital[i] #开仓后的净资金
    }
  }
  
  #每日结算
  if(position[i,1]!=position[i-1,1]){
    netcapital[i]<-netcapital[i]+position[i,1]*(IScloseprice[i,p1]-ISopenprice[i,p1])+position[i,2]*(IScloseprice[i,p2]-ISopenprice[i,p2])
  } else{
    netcapital[i]<-netcapital[i]+position[i,1]*(IScloseprice[i,p1]-IScloseprice[i-1,p1])+position[i,2]*(IScloseprice[i,p2]-IScloseprice[i-1,p2])
  }
  margin[i,1]<-abs(position[i,1]*IScloseprice[i,p1]*marginrate)
  margin[i,2]<-abs(position[i,2]*IScloseprice[i,p2]*marginrate)
}
}
#每日保证金合计
margin[,3]<-margin[,1]+margin[,2]

#资金曲线
plot(netcapital,type="l",ylab=paste(k))

#年化收益率
lagperiod<-max(min(which(IScloseprice[,p1]==IScloseprice[,p1][!is.na(IScloseprice[,p1])][1])),min(which(IScloseprice[,p2]==IScloseprice[,p2][!is.na(IScloseprice[,p2])][1])))
annualisedreturn[k]<-(netcapital[length(netcapital)]/netcapital[1])^(250/(length(netcapital)-lagperiod))-1

#最大回撤
drawdown<-c()
for(i in 1:length(netcapital)){
  drawdown[i]<-(netcapital[i]-min(netcapital[i:length(netcapital)]))/netcapital[i]
}
maxdrawdown[k]<-max(drawdown)

#年化波动率
annualisedvolatility[k]<-sd(diff(netcapital)/netcapital[-length(netcapital)])*sqrt(250)

#夏普率
sharperatio[k]<-(annualisedreturn[k]-0.06)/annualisedvolatility[k]

#交易次数
tradenumber[k]<-sum(transaction!=0)

#交易胜率
winrate[k]<-sum(tradeprofit>0,na.rm=TRUE)/(tradenumber[k]/2)

#平均每笔交易盈利
averagetradeprofit[k]<-mean(zerotoNA(tradeprofit),na.rm=TRUE)/(tradenumber[k]/2)

#打印指标
annualisedreturn[k]
maxdrawdown[k]
annualisedvolatility[k]
sharperatio[k]
tradenumber[k]
winrate[k]
averagetradeprofit[k]

sum(signal[!is.na(signal)]<=opendownline & signal[!is.na(signal)]>stoplossdownline)
sum(signal[!is.na(signal)]>=openupline & signal[!is.na(signal)]<stoplossupline)
sum(!is.na(signal))
}
#打印指标
summary<-cbind(annualisedreturn,maxdrawdown,annualisedvolatility,sharperatio,tradenumber,winrate,averagetradeprofit)