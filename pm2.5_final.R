Pm2.5模型建立
#-------------------------------------------------------------------
##建立時間序列資料
library('TSA')
pm2.5=read.csv("C:/pm2.52007-2016(mug.m3).csv",header=F)

pm2.5ts=ts(pm2.5[,1],start=c(2007,1),end=c(2015,12),frequency=12)

win.graph(width=4.875, height=3,pointsize=8)
plot(pm2.5ts,ylab='pm2.5',main='pm2.5 2007-2015 monthly average')
month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(pm2.5ts),pch=month)

#-------------------------------------------------------------------
##0.穩定模型

BoxCox.ar(pm2.5ts, lambda = seq(0, 1, 0.02))
pm2.5ts1=sqrt(pm2.5ts)

win.graph(width=4.875, height=3,pointsize=8)
plot(pm2.5ts1,ylab='sqrt(pm2.5)',main='sqrt(pm2.5) 2007-2015 monthly average',ylim=c(0,10))
month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(pm2.5ts1),pch=month)

acf(pm2.5ts1,lag.max=36,
main=expression(Sample~~ACF~~of~~pm2.5ts1~~Levels))

trend2 = lm(pm2.5ts1~time(pm2.5ts1))
summary(trend2)
yt = ts(rstudent(trend2),start=c(2007,1),frequency=12)
plot(yt,type='o',main='de-trended time series')
month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(yt),pch=month)
#-------------------------------------------------------------------
##a.判定模型 linear trend + SARIMA(0,0,2)*(0,1,1)
par(mfrow=c(1,1))
acf(yt,lag.max=36,
main='de=trended sqrt(pm2.5) 2007-2015 monthly average')
pacf(yt)

par(mfrow=c(1,2))
acf(diff(yt,lag=12),lag.max=36,ci.type='ma',
main='sqrt(pm2.5) 2007-2015 monthly average')
pacf(diff(yt,lag=12),lag.max=36,
main='sqrt(pm2.5) 2007-2015 monthly average')

eacf(diff(yt,lag=12))
#-------------------------------------------------------------------
##b.參數估計
m2.pm2.5=arima(yt,order=c(0,0,2),seasonal=list(order=c(0,1,1),period=12))
m2.pm2.5
#-------------------------------------------------------------------
##c.殘差分析
res2 = residuals(m2.pm2.5)
plot(res2)

par(mfrow=c(3,1))
plot(res2)
hist(res2)
qqnorm(res2); qqline(res2)
t.test(res2)
shapiro.test(res2)

par(mfrow=c(1,1))
acf(res2,lag = 36)
Box.test(res2, lag = 23,type = "Ljung")
#-------------------------------------------------------------------
##d.其他候選模型
##SARIMA(0,1,1)*(0,1,1)
pm2.5ts2=diff(pm2.5ts1)
acf(pm2.5ts2,lag.max=36,
main=expression(Sample~~ACF~~of~~pm2.5ts2~~Levels))

pm2.5ts3=diff(pm2.5ts2,lag=12)
acf(pm2.5ts3,lag.max=36,
main=expression(Sample~~ACF~~of~~pm2.5ts2~~Levels))

pacf(pm2.5ts3,lag.max=36,
main=expression(Sample~~ACF~~of~~pm2.5ts2~~Levels))

eacf(pm2.5ts3)

m1.pm2.5=arima(pm2.5ts1,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.pm2.5

m1.pm2.5=arima(pm2.5ts1,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.pm2.5

res1 = residuals(m1.pm2.5)
par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)
t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1,lag = 36)
Box.test(res1, lag = 15,type = "Ljung")
Box.test(res1, lag = 23,type = "Ljung")
#----------------------------------------------------------
#季節趨勢項+SARIMA
month.=season(pm2.5ts1)
trend1 = lm(pm2.5ts1~time(pm2.5ts1)+month.-1)
summary(trend1)

plot(y=rstudent(trend1),x=as.vector(time(pm2.5ts1)),ylab='De-trend series',
xlab='Time',type='o')
yt = rstudent(trend1)
adf.test(yt)

yt1=diff(yt)
adf.test(yt1)
plot(yt1,x=as.vector(time(pm2.5ts1)),ylab='De-trend series',
xlab='Time',type='o')

par(mfrow=c(2,1))
acf(yt,lag = 36)
pacf(yt,lag = 36)
eacf(yt)

model1 = arima(yt, order = c(1,0,0), include.mean = F)
model1

res1 = residuals(model1)
par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)
t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1,lag = 36)
Box.test(res1, lag = 23,type = "Ljung")

model2 = arima(yt, order = c(0,0,2), include.mean = F)
model2

res2 = residuals(model2)
par(mfrow=c(3,1))
plot(res2)
hist(res2)
qqnorm(res2); qqline(res2)

par(mfrow=c(1,1))
acf(res2,lag = 36)
#-------------------------------------------------------------------
##預測
par(mfrow=c(1,1))
pm2.511=pm2.5[,1]

testpm2.5 = ts(pm2.511[(9*12+1):(10*12)],start=c(2016,1),frequency=12)
testpm2.5=(testpm2.5)^(1/2)

m3.pm2.5=arima(pm2.5ts1,order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12)
,xreg=as.matrix(model.matrix(~time(pm2.5ts1)))[,-1])

newtrend = time(pm2.5ts1)[length(pm2.5ts1)]+(1:12)*deltat(pm2.5ts1)

plot(m3.pm2.5,n1=c(2007,1),n.ahead=12,newxreg = as.matrix(model.matrix(~newtrend))[,-1]
,col='red',xlab='Year',type='o', ylab='sqrt(pm2.5)',
main=expression(Long~~Term~~Forecasts~~'for'~~the~~pm2.5~~Model))
points(testpm2.5,col = "blue", pch=3)










