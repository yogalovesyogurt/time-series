library(xts)
library(quantmod)
library(tseries)
library(TSA)
library(fGarch)
sp500=read.csv("E:\\统计\\金融统计\\Garch\\data\\GSPC.csv",header = TRUE)
facebook=read.csv("E:\\统计\\金融统计\\Garch\\data\\FB.csv",header = TRUE)
morgan=read.csv("E:\\统计\\金融统计\\Garch\\data\\JPM.csv",header = TRUE)
sp.future=read.csv("E:\\统计\\金融统计\\Garch\\data\\future.csv",header = TRUE)[2:14,c(1,5)]
sp500=sp500[,c(1,5,6)]
facebook=facebook[,c(1,5,6)]
morgan=morgan[,c(1,5,6)]
t=sp500[,1]
tfu=sp.future[,1]
sp.close=xts(sp500[,2],order.by = as.Date(t))
fb.close=xts(facebook[,2],order.by = as.Date(t))
jpm.close=xts(morgan[,2],order.by = as.Date(t))
sp.future.close=xts(sp.future[,2],order.by = as.Date(tfu))

sp.close.adj=xts(sp500[,3],order.by = as.Date(t))
fb.close.adj=xts(facebook[,3],order.by = as.Date(t))
jpm.close.adj=xts(morgan[,3],order.by = as.Date(t))

#calculate daily return
sp.return=periodReturn(sp.close,period="daily",type="log")[2:251]
fb.return=periodReturn(fb.close,period="daily",type="log")[2:251]
jpm.return=periodReturn(jpm.close,period="daily",type="log")[2:251]
sp.future.return=periodReturn(sp.future.close,period = 'daily',type = 'log')[2:13]

#figures 
par(mfrow=c(3,1))
plot(sp.close,main = 'Daily Closing Price of S&P500 index',lwd='0.5',col='red')
plot(fb.close,main = 'Daily Closing Price of Facebook Stock',lwd='0.5',col='blue')
plot(jpm.close,main = 'Daily Closing Price of JPMorgan Stock',lwd='0.5',col='blue')

par(mfrow=c(3,1))
plot(sp.close.adj,main = 'Daily Adjusted-Closing Price of S&P500 index',lwd='0.5',col='red')
plot(fb.close.adj,main = 'Daily Adjusted-Closing Price of Facebook Stock',lwd='0.5',col='blue')
plot(jpm.close.adj,main = 'Daily Adjusted-Closing Price of JPMorgan Stock',lwd='0.5',col='blue')

par(mfrow=c(3,1))
plot(sp.return,main = 'Daily Return of S&P500 index',lwd='0.5',col='red')
plot(fb.return,main = 'Daily Return of Facebook Stock',lwd='0.5',col='blue')
plot(jpm.return,main = 'Daily Return of JPMorgan Stock',lwd='0.5',col='blue')

#test of the mean
t.test(as.vector(sp.return))
t.test(as.vector(fb.return))
t.test(as.vector(jpm.return))

#acf&pacf
par(mfrow=c(2,2))
acf(sp.return,main="ACF of Daily Return of S&P500 index",drop.lag.0=F)
acf(sp.return^2,main="ACF of Squared Daily Return of S&P500 index",drop.lag.0=F)
pacf(sp.return,main="PACF of Daily Return of S&P500 index")
pacf(sp.return^2,main="PACF of Squared Daily Return of S&P500 index")
par(mfrow=c(2,2))
acf(fb.return,main="ACF of Daily Return of Facebook Stock",drop.lag.0=F)
acf((fb.return)^2,main="ACF of Squared Daily Return of Facebook Stock",drop.lag.0=F)
pacf(fb.return,main="PACF of Daily Return of Facebook Stock")
pacf(fb.return^2,main="PACF of Squared Daily Return of Facebook Stock")
par(mfrow=c(2,2))
acf(jpm.return,main="ACF of Daily Return of JPMorgan Stock",drop.lag.0=F)
acf(jpm.return^2,main="ACF of Squared Daily Return of JPMorgan Stock",drop.lag.0=F)
pacf(jpm.return,main="PACF of Daily Return of JPMorgan Stock")
pacf(jpm.return^2,main="PACF of Squared Daily Return of JPMorgan Stock")

Box.test(sp.return,lag = 12,type = 'Ljung-Box')
Box.test(fb.return,lag = 12,type = 'Ljung-Box')
Box.test(jpm.return,lag = 12,type = 'Ljung-Box')

#Arch test
Box.test((sp.return-mean(sp.return))^2,lag = 12,type = 'Ljung-Box')
Box.test((fb.return-mean(fb.return))^2,lag = 12,type = 'Ljung-Box')
Box.test((jpm.return-mean(jpm.return))^2,lag = 12,type = 'Ljung-Box')
library(FinTS)   #LM检验
ArchTest(sp.return,lag=12)
ArchTest(fb.return,lag=12)
ArchTest(jpm.return,lags = 12)

###############################################################################
#garch model of s&p500
#normal
sp.garch11=garchFit(~1+garch(1,1),data = sp.return,trace = F)
summary(sp.garch11)
#plot(sp.garch11)
sp.garch12=garchFit(~1+garch(1,2),data = sp.return,trace = F)
summary(sp.garch12)
sp.garch21=garchFit(~1+garch(2,1),data = sp.return,trace = F)
summary(sp.garch21)
vola=volatility(sp.garch11)
hatmu = coef(sp.garch11)["mu"]
lb.intel = hatmu - 2*vola
ub.intel = hatmu + 2*vola
sp.res=residuals(sp.garch11, standardize=TRUE)
plot(sp.garch11,which=13)
par(mfrow=c(1,2))
plot(sp.return,lwd='0.5',main='Daily Return of S&P500 index')
lines(xts(ub.intel,order.by = as.Date(t[2:251])),col='red')
lines(xts(lb.intel,order.by = as.Date(t[2:251])),col='red')
qqnorm(sp.res)
qqline(sp.res,col='red')

par(mfrow=c(1,2))
acf(sp.res^2,main="Squared residuals for S&P 500",drop.lag.0=F)
acf(abs(sp.res),main="Absolute residuals for S&P 500",drop.lag.0=F)

#sp.bp=vola*sp.res
#par(mfrow=c(1,1))
#plot(sp.return,lwd='0.5',main='Daily Return of S&P500 index')
#lines(xts(sp.bp,order.by = as.Date(t[2:251])),col='red')

##VaR for normal
VaR1.norm=vola*qnorm(0.05)
VaR1.empirical=vola*quantile(sp.res, 0.05)
VaR2.norm=vola*qnorm(0.025)
VaR2.empirical=vola*quantile(sp.res,0.025)
par(mfrow=c(1,1))
plot(sp.return,lwd='0.5',main='Daily Return of S&P500 index in 2018')
lines(xts(VaR1.norm,order.by = as.Date(t[2:251])),col='red')
lines(xts(VaR2.norm,order.by = as.Date(t[2:251])),col='darkgreen')
lines(xts(VaR1.empirical,order.by = as.Date(t[2:251])),col='cornflowerblue')
lines(xts(VaR2.empirical,order.by = as.Date(t[2:251])),col='orangeRed')

#t distribution
sp.garch.t=garchFit(~1+garch(1,1),data = sp.return,cond.dist="std",trace = F)
summary(sp.garch.t)
sp.res.t=residuals(sp.garch.t,standardize=T)
vola.t=volatility(sp.garch.t)
hatmu.t = coef(sp.garch.t)["mu"]
lb.intel.t = hatmu.t - 2*vola.t
ub.intel.t = hatmu.t + 2*vola.t
plot(sp.garch.t,which=13)
par(mfrow=c(1,2))
plot(sp.return,lwd='0.5',main='Daily Return of S&P500 index')
lines(xts(ub.intel.t,order.by = as.Date(t[2:251])),col='red')
lines(xts(lb.intel.t,order.by = as.Date(t[2:251])),col='red')
qqplot(rt(250,df=coef(sp.garch.t)["shape"]),sp.res.t,ylab ='Sample Quantiles',xlab = 'Theoretical Quantiles' )
qqline(rt(250,df=coef(sp.garch.t)["shape"]),sp.res.t,col='red')

#summary(garchFit(~1+garch(1,2),data = sp.return,cond.dist="std",trace = F))
#summary(garchFit(~1+garch(2,1),data = sp.return,cond.dist="std",trace = F))
par(mfrow=c(1,2))
acf(sp.res.t^2,main="Squared residuals for S&P 500",drop.lag.0=F)
acf(abs(sp.res.t),main="Absolute residuals for S&P 500",drop.lag.0=F)


ks.test(sp.res.t,pt,df=coef(sp.garch.t)["shape"])

#ks.test(residuals(sp.garch.st),pt,df=coef(sp.garch.st)["shape"],ncp=coef(sp.garch.st)["shape"])
ks.test(residuals(sp.garch.st),pnorm,mean=coef(sp.garch.st)["skew"])
##VaR for t
VaR1.t=vola.t*qt(0.05,df=coef(sp.garch.t)["shape"])
VaR1.empirical.t=vola.t*quantile(sp.res.t, 0.05)
VaR2.t=vola.t*qt(0.025,df=coef(sp.garch.t)["shape"])
VaR2.empirical.t=vola.t*quantile(sp.res.t,0.025)
par(mfrow=c(1,1))
plot(sp.return,lwd='0.5',main='Daily Return of S&P500 index in 2018')
lines(xts(VaR1.t,order.by = as.Date(t[2:251])),col='blueviolet')
lines(xts(VaR2.t,order.by = as.Date(t[2:251])),col='darkgreen')
lines(xts(VaR1.empirical.t,order.by = as.Date(t[2:251])),col='cornflowerblue')
lines(xts(VaR2.empirical.t,order.by = as.Date(t[2:251])),col='orangeRed')

##skew t
sp.garch.st=garchFit(~1+garch(1,1),data = sp.return,cond.dist="sstd",trace = F)
sp.res.st=residuals(sp.garch.st,standardize=T)
summary(sp.garch.st)
plot(sp.garch.st,which=13)
vola.st=volatility(sp.garch.st)
hatmu.st = coef(sp.garch.st)["mu"]
lb.intel.st = hatmu.st - 2*vola.st
ub.intel.st = hatmu.st + 2*vola.st
par(mfrow=c(1,2))
plot(sp.return,lwd='0.5',main='Daily Return of S&P500 index')
lines(xts(ub.intel.st,order.by = as.Date(t[2:251])),col='red')
lines(xts(lb.intel.st,order.by = as.Date(t[2:251])),col='red')
qqplot(rt(250,df=coef(sp.garch.st)["shape"],ncp =-coef(sp.garch.st)["skew"]),sp.res.st,ylab ='Sample Quantiles',xlab = 'Theoretical Quantiles' )
qqline(rt(250,df=coef(sp.garch.st)["shape"],ncp =-coef(sp.garch.st)["skew"]),sp.res.st,col='red')

##VaR for st
VaR1.st=vola.st*qt(0.05,df=coef(sp.garch.st)["shape"])
VaR1.empirical.st=vola.st*quantile(sp.res.st, 0.05)
VaR2.st=vola.st*qt(0.025,df=coef(sp.garch.st)["shape"])
VaR2.empirical.st=vola.st*quantile(sp.res.st,0.025)
par(mfrow=c(1,1))
plot(sp.return,lwd='0.5',main='Daily Return of S&P500 index in 2018')
lines(xts(VaR1.st,order.by = as.Date(t[2:251])),col='blueviolet')
lines(xts(VaR2.st,order.by = as.Date(t[2:251])),col='darkgreen')
lines(xts(VaR1.empirical.st,order.by = as.Date(t[2:251])),col='cornflowerblue')
lines(xts(VaR2.empirical.st,order.by = as.Date(t[2:251])),col='orangeRed')

par(mfrow=c(1,2))
acf(sp.res.st^2,main="Squared residuals for S&P 500",drop.lag.0=F)
acf(abs(sp.res.st),main="Absolute residuals for S&P 500",drop.lag.0=F)

ks.test(sp.res.st,pt,df=coef(sp.garch.st)["shape"])

sp.p1 = predict(sp.garch11, n.ahead=12)[["standardDeviation"]]
sp.p2 = predict(sp.garch.t, n.ahead=12)[["standardDeviation"]]
sp.p3=predict(sp.garch.st, n.ahead=12)[["standardDeviation"]]
library(tibble)
pred.tab = tibble(
  "预测步数"=1:12,
  "正态"=sp.p1,
  "对称t"=sp.p2,
  "有偏t"=sp.p3
)
knitr::kable(pred.tab, digits=4)

plot(xts(sp.p1,order.by = as.Date(tfu[2:13])))
mse1=0
for (i in 1:500) {
  mse1=mse1+sum((abs(as.vector(sp.future.return)/sample(sp.res,12))-sp.p1)^2)/12
}
mse1=mse1/500

mse2=0
for (i in 1:500) {
  mse2=mse2+sum((abs(as.vector(sp.future.return)/sample(sp.res.t,12))-sp.p2)^2)/12
  i=i+1
}
mse2=mse2/500

mse3=0
for (i in 1:500) {
  mse3=mse3+sum((abs(as.vector(sp.future.return)/sample(sp.res.st,12))-sp.p3)^2)/12
}
mse3=mse3/500

####################################################################################
#garch model of fb
fb.garch11=garchFit(~1+garch(1,1),data = fb.return,trace = F)
summary(fb.garch11)
fb.garch12=garchFit(~1+garch(1,2),data = fb.return,trace = F)
summary(fb.garch12)
fb.garch21=garchFit(~1+garch(2,1),data = fb.return,trace = F)
summary(fb.garch21)

fb.vola=volatility(fb.garch11)
fb.hatmu = coef(fb.garch11)["mu"]
fb.lb.intel = fb.hatmu - 2*fb.vola
fb.ub.intel = fb.hatmu + 2*fb.vola
fb.res=residuals(fb.garch11, standardize=TRUE)
#plot(fb.garch11,which=13)
par(mfrow=c(1,2))
plot(fb.return,lwd='0.5',main='Daily Return of Facebook stock in 2018')
lines(xts(fb.ub.intel,order.by = as.Date(t[2:251])),col='red')
lines(xts(fb.lb.intel,order.by = as.Date(t[2:251])),col='red')
qqnorm(fb.res)
qqline(fb.res,col='red')

par(mfrow=c(1,2))
acf(fb.res^2,main="Squared residuals for Facebook stock",drop.lag.0=F)
acf(abs(fb.res),main="Absolute residuals for Facebook stock",drop.lag.0=F)

##VaR for normal
fb.VaR1.norm=fb.vola*qnorm(0.05)
fb.VaR1.empirical=fb.vola*quantile(fb.res, 0.05)
fb.VaR2.norm=fb.vola*qnorm(0.025)
fb.VaR2.empirical=fb.vola*quantile(fb.res,0.025)
par(mfrow=c(1,1))
plot(fb.return,lwd='0.5',main='Daily Return of Facebook stock in 2018')
lines(xts(fb.VaR1.norm,order.by = as.Date(t[2:251])),col='red')
lines(xts(fb.VaR2.norm,order.by = as.Date(t[2:251])),col='darkgreen')
lines(xts(fb.VaR1.empirical,order.by = as.Date(t[2:251])),col='cornflowerblue')
lines(xts(fb.VaR2.empirical,order.by = as.Date(t[2:251])),col='orangeRed')

#t distribution
fb.garch.t=garchFit(~1+garch(1,1),data = fb.return,cond.dist="std",trace = F)
summary(fb.garch.t)
fb.res.t=residuals(fb.garch.t,standardize=T)
fb.vola.t=volatility(fb.garch.t)
fb.hatmu.t = coef(fb.garch.t)["mu"]
fb.lb.intel.t = fb.hatmu.t - 2*fb.vola.t
fb.ub.intel.t = fb.hatmu.t + 2*fb.vola.t
plot(fb.garch.t,which=13)
par(mfrow=c(1,2))
plot(fb.return,lwd='0.5',main='Daily Return of Facebook stock')
lines(xts(fb.ub.intel.t,order.by = as.Date(t[2:251])),col='red')
lines(xts(fb.lb.intel.t,order.by = as.Date(t[2:251])),col='red')
qqplot(rt(250,df=coef(fb.garch.t)["shape"]),fb.res.t,ylab ='Sample Quantiles',xlab = 'Theoretical Quantiles' )
qqline(rt(250,df=coef(fb.garch.t)["shape"]),fb.res.t,col='red')

par(mfrow=c(1,2))
acf(fb.res.t^2,main="Squared residuals for Facebook stock",drop.lag.0=F)
acf(abs(fb.res.t),main="Absolute residuals for Facebook stock",drop.lag.0=F)

ks.test(fb.res.t,pt,df=coef(fb.garch.t)["shape"])

##VaR for t
fb.VaR1.t=fb.vola.t*qt(0.05,df=coef(fb.garch.t)["shape"])
fb.VaR1.empirical.t=fb.vola.t*quantile(fb.res.t, 0.05)
fb.VaR2.t=fb.vola.t*qt(0.025,df=coef(fb.garch.t)["shape"])
fb.VaR2.empirical.t=fb.vola.t*quantile(fb.res.t,0.025)
par(mfrow=c(1,1))
plot(fb.return,lwd='0.5',main='Daily Return of Facebook stock in 2018')
lines(xts(fb.VaR1.t,order.by = as.Date(t[2:251])),col='blueviolet')
lines(xts(fb.VaR2.t,order.by = as.Date(t[2:251])),col='darkgreen')
lines(xts(fb.VaR1.empirical.t,order.by = as.Date(t[2:251])),col='cornflowerblue')
lines(xts(fb.VaR2.empirical.t,order.by = as.Date(t[2:251])),col='orangeRed')

fb.p1 = predict(fb.garch11, n.ahead=12)[["standardDeviation"]]
fb.p2 = predict(fb.garch.t, n.ahead=12)[["standardDeviation"]]

pred.tab.fb = tibble(
  "预测步数"=1:12,
  "正态"=fb.p1,
  "对称t"=fb.p2,
)
knitr::kable(pred.tab.fb, digits=3)
###################################################################################
#garch model of jpm
jpm.garch11=garchFit(~1+garch(1,1),data = jpm.return,trace = F)
summary(jpm.garch11)
jpm.garch12=garchFit(~1+garch(1,2),data = jpm.return,trace = F)
summary(jpm.garch12)
jpm.garch21=garchFit(~1+garch(1,1),data = jpm.return,trace = F)
summary(jpm.garch21)

jpm.vola=volatility(jpm.garch11)
jpm.hatmu = coef(jpm.garch11)["mu"]
jpm.lb.intel = jpm.hatmu - 2*jpm.vola
jpm.ub.intel = jpm.hatmu + 2*jpm.vola
jpm.res=residuals(jpm.garch11, standardize=TRUE)
#plot(fb.garch11,which=13)
par(mfrow=c(1,2))
plot(jpm.return,lwd='0.5',main='Daily Return of J.P.Morgan stock in 2018')
lines(xts(jpm.ub.intel,order.by = as.Date(t[2:251])),col='red')
lines(xts(jpm.lb.intel,order.by = as.Date(t[2:251])),col='red')
qqnorm(jpm.res)
qqline(jpm.res,col='red')

par(mfrow=c(1,2))
acf(jpm.res^2,main="Squared residuals for J.P.Morgan stock",drop.lag.0=F)
acf(abs(jpm.res),main="Absolute residuals for J.P.Morgan stock",drop.lag.0=F)

##VaR for normal
jpm.VaR1.norm=jpm.vola*qnorm(0.05)
jpm.VaR1.empirical=jpm.vola*quantile(jpm.res, 0.05)
jpm.VaR2.norm=jpm.vola*qnorm(0.025)
jpm.VaR2.empirical=jpm.vola*quantile(jpm.res,0.025)
par(mfrow=c(1,1))
plot(jpm.return,lwd='0.5',main='Daily Return of J.P.Morgan stock in 2018')
lines(xts(jpm.VaR1.norm,order.by = as.Date(t[2:251])),col='red')
lines(xts(jpm.VaR2.norm,order.by = as.Date(t[2:251])),col='darkgreen')
lines(xts(jpm.VaR1.empirical,order.by = as.Date(t[2:251])),col='cornflowerblue')
lines(xts(jpm.VaR2.empirical,order.by = as.Date(t[2:251])),col='orangeRed')

#t distribution
jpm.garch.t=garchFit(~1+garch(1,1),data = jpm.return,cond.dist="std",trace = F)
summary(jpm.garch.t)
jpm.res.t=residuals(jpm.garch.t,standardize=T)
jpm.vola.t=volatility(jpm.garch.t)
jpm.hatmu.t = coef(jpm.garch.t)["mu"]
jpm.lb.intel.t = jpm.hatmu.t - 2*jpm.vola.t
jpm.ub.intel.t = jpm.hatmu.t + 2*jpm.vola.t
par(mfrow=c(1,2))
plot(jpm.return,lwd='0.5',main='Daily Return of J.P.Morgan stock')
lines(xts(jpm.ub.intel.t,order.by = as.Date(t[2:251])),col='red')
lines(xts(jpm.lb.intel.t,order.by = as.Date(t[2:251])),col='red')
qqplot(rt(250,df=coef(jpm.garch.t)["shape"]),jpm.res.t,ylab ='Sample Quantiles',xlab = 'Theoretical Quantiles' )
qqline(rt(250,df=coef(jpm.garch.t)["shape"]),jpm.res.t,col='red')

par(mfrow=c(1,2))
acf(jpm.res.t^2,main="Squared residuals for J.P.Morgan stock",drop.lag.0=F)
acf(abs(jpm.res.t),main="Absolute residuals for J.P.Morgan stock",drop.lag.0=F)

ks.test(jpm.res.t,pt,df=coef(jpm.garch.t)["shape"])

##VaR for t
jpm.VaR1.t=jpm.vola.t*qt(0.05,df=coef(jpm.garch.t)["shape"])
jpm.VaR1.empirical.t=jpm.vola.t*quantile(jpm.res.t, 0.05)
jpm.VaR2.t=jpm.vola.t*qt(0.025,df=coef(jpm.garch.t)["shape"])
jpm.VaR2.empirical.t=jpm.vola.t*quantile(jpm.res.t,0.025)
par(mfrow=c(1,1))
plot(jpm.return,lwd='0.5',main='Daily Return of J.P.Morgan stock in 2018')
lines(xts(jpm.VaR1.t,order.by = as.Date(t[2:251])),col='blueviolet')
lines(xts(jpm.VaR2.t,order.by = as.Date(t[2:251])),col='darkgreen')
lines(xts(jpm.VaR1.empirical.t,order.by = as.Date(t[2:251])),col='cornflowerblue')
lines(xts(jpm.VaR2.empirical.t,order.by = as.Date(t[2:251])),col='orangeRed')

jpm.p1 = predict(jpm.garch11, n.ahead=12)[["standardDeviation"]]
jpm.p2 = predict(jpm.garch.t, n.ahead=12)[["standardDeviation"]]
pred.tab.jpm = tibble(
  "预测步数"=1:12,
  "正态"=fb.p1,
  "对称t"=fb.p2,
)
knitr::kable(pred.tab.jpm, digits=3)
