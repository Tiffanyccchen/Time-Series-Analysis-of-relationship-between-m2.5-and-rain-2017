Correlation
#-------------------------------------------------------------------
##Pm2.5和降雨量殘差分析
respm2.5=residuals(m2.pm2.5)
resrain=residuals(model)
pm2.5rain=ts.intersect(respm2.5,resrain)
plot(pm2.5rain,main="Pm2.5 and Rain Residual Plot ")

#-------------------------------------------------------------------
##CCF分析
pm2.5=pm2.5rain[,1]
rain=pm2.5rain[,2]
ccf(as.vector(pm2.5),as.vector(rain),ylab='ccf',main='x=pm2.5,y=rain')
#-------------------------------------------------------------------
##pm2.5往後移動7步後兩模型殘差時間序列圖
resrainnew=ts(rain[1:101],start=c(2007,1),end=c(2015,5),frequency=12)
respm2.5new=ts(pm2.5[8:108],start=c(2007,8),end=c(2015,12),frequency=12)
pm2.5rainnew=ts.intersect(respm2.5new,resrainnew)
plot(pm2.5rainnew,main="經過d=7移動，pm2.5月均濃度和月均雨量的殘差時間序列圖")
#-------------------------------------------------------------------
##配適回歸模型
mode1final=lm(respm2.5new~ resrainnew)
summary(mode1final)
mode1final=lm(respm2.5new~ resrainnew)
res<-summary(mode1final)$residuals
#-------------------------------------------------------------------
##殘差模型配適
plot(res, type= "o ")
acf(res)
pacf(res)
eacf(res)
#-------------------------------------------------------------------
##殘差診斷
par(mfrow=c(3,1))
plot(res,type= "o ")
hist(res)
qqnorm(res); qqline(res)
t.test(res)
shapiro.test(res)

par(mfrow=c(1,1))
acf(res,lag = 36)
Box.test(res, lag = 7,type = "Ljung")
#-------------------------------------------------------------------
##相關性圖
par(mfrow=c(1,3))
plot(as.numeric(resrain),as.numeric(respm2.5))
cor(respm2.5,resrain)
abline(lm(as.numeric(resrain)~as.numeric(respm2.5)))

plot(as.numeric(resrainnew),as.numeric(respm2.5new))
cor(respm2.5new,resrainnew)
abline(lm(as.numeric(respm2.5new)~as.numeric(resrainnew)))

plot(as.numeric(raints),as.numeric(pm2.5ts))
abline(lm(as.numeric(pm2.5ts)~as.numeric(raints)))
cor(as.numeric(pm2.5ts),as.numeric(raints))

