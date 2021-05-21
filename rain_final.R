降雨量模型建立
#-------------------------------------------------------------------
##建立時間序列資料
rain=read.csv("C:/rain2007-2016(mm3).csv",header=F)
raints=ts(rain[,1],start=c(2007,1),end=c(2015,12),frequency=12)

win.graph(width=4.875, height=3,pointsize=8)
plot(raints,ylab='CO2',main='rain 2007-2015 monthly average')
month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(raints),pch=month)
#-------------------------------------------------------------------
##穩定模型
BoxCox.ar(raints, lambda = seq(0, 1, 0.02))
raints1=(raints)^(1/4)

plot(raints1,ylab='CO2',main='rain 2007-2015 monthly average')
month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(raints1),pch=month)

month.=season(raints1)
trend1 = lm(raints~time(raints1)+month.-1)
summary(trend1)

model = lm(raints1~month.-1)
summary(model)

plot(y=rstudent(model),x=as.vector(time(raints1)),ylab='De-trend series',
xlab='Time',type='o')
yt = rstudent(model)
#-------------------------------------------------------------------
##a.模型判定
par(mfrow=c(2,1))
acf(yt,lag = 36)
pacf(yt,lag = 36)
eacf(yt)
#-------------------------------------------------------------------
##b.參數估計
model = lm(raints1~month.)
summary(model)

model = lm(raints1~month.-1)
summary(model)
#-------------------------------------------------------------------
##c.殘差分析
res1 = residuals(model)

par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)
t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1,lag = 36)
Box.test(res1, lag = 8,type = "Ljung")
#-------------------------------------------------------------------
##d.其他候選模型 SARIMA(0,1,1)

acf(raints1,lag.max=48,
main=expression(Sample~~ACF~~of~~pm2.5ts1~~Levels))
pacf(raints1,lag.max=48,
main=expression(Sample~~ACF~~of~~pm2.5ts1~~Levels))
eacf(raints1)

raints2=diff(raints1,lag=12)
plot(raints2,ylab='CO2',main='rain 2007-2015 monthly average')
month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(raints2),pch=month)

acf(raints2,lag.max=48,
main=expression(Sample~~ACF~~of~~pm2.5ts1~~Levels))
pacf(raints2,lag.max=48,
main=expression(Sample~~ACF~~of~~pm2.5ts1~~Levels))
eacf(raints2)

m1.rain=arima(raints1,seasonal=list(order=c(0,1,1),period=12))
m1.rain

res1 = residuals(m1.rain)
par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)
t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1,lag = 36)
Box.test(res1, lag = 8,type = "Ljung")
#----------------------------------------------------------------------
##預測
rain11=rain[,1]
testrain = ts(rain11[(9*12+1):(10*12)],start=c(2016,1),frequency=12)
testrain1=(testrain)^(1/4)

par(mfrow=c(1,1))
rain11=rain[,1]
xreg <-as.matrix(model.matrix(~month.-1))[,-1]
model.1<-arima(raints1,order=c(0,0,0),xreg=xreg)

newmonth.=season(ts(rep(1,12),start=c(2016,1),freq=12))
newtrend=(1:12)*deltat(raints1)
xreg.1 <-as.matrix(model.matrix(~newmonth.))[,-1]

win.graph(width=6.5,height=3,pointsize=8)
plot(model.1,n.ahead=12,n1=c(2007,1),xlab="Year",pch=19,ylab='(rain)^1/4',
main="Long Term Forecasts for rain",newxreg=xreg.1,type='o')
points(testrain1,col = "blue", pch=3)

#----------------------------------------------------------------------
##季節趨勢項模型&SARIMA(0,1,1)MSE比較
a<-as.vector(plot(model.1,n.ahead=12,n1=c(2007,1),xlab="Year",pch=19,ylab='(rain)^1/4',main="Long Term Forecasts for rain",newxreg=xreg.1,type='o')$pred)

win.graph(width=6.5,height=3,pointsize=8)
plot(m1.rain,n1=c(2007,1),n.ahead=12,col='red',xlab='Year',type='o',
ylab='(rain)^1/4',
main="Long Term Forecasts for rain")
points(testrain1,col = "blue", pch=3)

b<-as.vector(plot(m1.rain,n1=c(2007,1),n.ahead=12,col='red',xlab='Year',type='o',
ylab='(rain)^1/4',main="Long Term Forecasts for rain")$pred)

y=testrain1
y
sum((a-y)^2)
sum((b-y)^2)
