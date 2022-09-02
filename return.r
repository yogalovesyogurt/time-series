library(xts)
a=read.csv("E:\\统计\\金融统计\\2018 s&p500.csv",header = TRUE)
b=read.csv("E:\\统计\\金融统计\\DIS.csv",header = TRUE)
c=read.csv("E:\\统计\\金融统计\\future.csv",header = TRUE)
da=a[,c(1,5)]#选取时间及闭市价格
dadis=b[1:250,c(1,5)]
da_future=c[,c(1,5)]
class(da)#数据类型
t=da[,1]
cp=da[,2]#选取闭市价格
acf(df,main="ACF of The Close Price")
pacf(cp,main="PACF of The Close Price")
cpdis=dadis[,2]

week=xts(da_future[1:6,2],order.by = as.Date(da_future[1:6,1]))
month=xts(da_future[,2],order.by = as.Date(da_future[,1]))
week_return=periodReturn(week,period="daily",type="log")
month_return=periodReturn(month,period="daily",type="log")
length(month_return)

df=xts(cp,order.by=as.Date(da[,1]))#时间序列
dfdis=xts(cpdis,order.by = as.Date(dadis[,1]))
acf(dfdis)
chartSeries(dfdis,theme="white",up.col = "black",name = "2018年DISNEY价格")
chartSeries(df,theme="white",up.col = "black",name = "2018年s&p500价格")
library(quantmod)
rate=periodReturn(df,period="daily",type="log")#收益率
ratedis=periodReturn(dfdis,period="daily",type = "log")

chartSeries(rate,theme="white",up.col = "black",name = "2018年s&p500收益率")
chartSeries(ratedis,theme="white",up.col = "black",name = "2018年DISNEY收益率")

acf(rate,main="ACF of The Return")#收益率自相关
pacf(rate,main="PACF of The Return")#偏自相关

pic=eacf(rate,15,15)
pic$symbol
library(tseries)
library(TSA)
adf.test(df)
adf.test(rate)
adf.test(ratedis)
acf(ratedis,main="ACF of The Return of The Disney")
pacf(ratedis,main="PACF of The Return of The Disney")
library(stats)
library(forecast)
auto.arima(rate)
auto.arima(ratedis)
model=stats::arima(rate,order = c(2,0,2))
model
modeldis=stats::arima(ratedis,order = c(0,0,0))
Fore=forecast(model,h=5,level=c(99.5))
Fore
plot(Fore)
week_return$daily.returns
Fore$mean
norm(matrix(c(0.008456626-0.004235553,0.001267693+0.002130021,-0.025068332-0.003124914,0.033759399+0.001656161,0.006985976-0.002285850)))
lines(week_return)
#一周预测
prew=xts(predict(model,5)$pred,order.by = as.Date(da_future[2:6,1]))
plot(week_return[2:6],main = "The Comparation of the Prediction and the Real Data",col='2',lwd='0.5')
lines(prew,col = '1')
legend("top",legend=c("real return","estimated return"),col=c("red","black"),lty=1,lwd=2)
#一月预测
premm=xts(predict(model,22)$pred,order.by = as.Date(da_future[2:23,1]))
plot(month_return[2:23],main = "The Comparation of the Prediction and the Real Data",col='2',lwd='0.5')
lines(premm,col = '1')
prem=predict(model,22)$pred
matrix(prem)
matrix(month_return[2:23])
norm(matrix(prem)-matrix(month_return[2:23]))

tsdiag(model)
tsdiag(modeldis)


qqnorm(model$residuals)
qqline(model$residuals)

Box.test(model$residuals,lag=6,type="Ljung-Box")
Box.test(model$residuals,lag=12,type="Ljung-Box")
Box.test(model$residuals,lag=18,type="Ljung-Box")

#plot(rstandard(model),type="o")#残差曲线图
plot(xts(model$residuals,order.by=as.Date(da[,1])),main = 'Residuals of ARMA(2,2)',lwd = 0.5,col='2')#残差曲线图
plot(modeler$residuals,type="l")

#线性拟合
lms=lm(rate~ratedis,data=data.frame(rate,ratedis));
summary(lms)
cc=lms$residuals
plot(cc[,1],type = 'l',main = 'Residuals of Linear Fit',lwd = 0.5)

auto.arima(lms$residuals)
eacf(lms$residuals)
modeler=arima(lms$residuals,order = c(0,0,0))
modeler
tsdiag(modeler)
qqnorm(modeler$residuals)
qqline(modeler$residuals)
Box.test(modeler$residuals,lag=6,type="Ljung-Box")
Box.test(modeler$residuals,lag=12,type="Ljung-Box")
Box.test(modeler$residuals,lag=18,type="Ljung-Box")

# model1=stats::arima(rate,order = c(0,0,0))
# model1
# plot(forecast(model1,h=5,level=c(99.5)))
# tsdiag(model1)
# qqnorm(model1$residuals)
# qqline(model1$residuals)
# Box.test(model1$residuals,type="Ljung-Box")

#FF模型
d=read.csv("E:\\统计\\金融统计\\FF.csv",header = TRUE)
rm=d[,2]
rf=d[,5]
SMB=d[,3]
HML=d[,4]
rmt=xts(rm,order.by = as.Date(t))
rft=xts(rf,order.by = as.Date(t))
SMBt=xts(SMB,order.by = as.Date(t))
HMLt=xts(HML,order.by = as.Date(t))
rgroup=rmt-rft
y=ratedis-rft
lmf=lm(y~rgroup+SMBt+HMLt)
summary(lmf)
plot(lmf$residuals[,1],type='p',main='The Residuals of Model Fama-French')

