data<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/ABFRL.NS (monthly).csv")
head(data)
tail(data)
data_close<-ts(data$Close,start=2014,frequency=12)
b=data_close
str(data)
sum(is.na(data_close))
plot(data_close,col="blue",xlab="time",ylab="Close-Price",main="Aditya Birla MF")
dcom<-decompose(data_close)
plot(dcom)
library(forecast)
library(tseries)
library(zoo)
adf.test(data_close)
auto.arima(data_close)
# since p-value=0.30>0.05, so we are fail to reject H_o, i.e the series is non-stationary
y=diff(data_close)
auto.arima(y)
str(y)
adf.test(y)
# Here p-value<0.05, the series y is stationary
plot(y)
acf(y,lag.max=100,main="ACF")
pacf(y,lag.max=50,main="Pacf")
#Seasonal Differencing
plot(diff(y,12))
t<-diff(y,12)
plot(t)
adf.test(t)
acf(t,lag.max=100,main="ACF")
pacf(t,lag.max=100,main="PACF")
##fitting the model
flag=10^7
v=rep(0,7)
for(p in 0:3){
  for(q in 0:3){
    for(P in 0:3){
      for( Q in 0:3){
        model<-arima(x=data_close,order=c(p,1,q),seasonal =list(order=c(P,1,Q),frequency=12))
        sse=sum((model$residuals)^2)
        if(flag>model$aic){
          flag=model$aic
          v=c(p,1,q,P,1,Q)
        }
      }
    }
    
  }
  
}
v 


###Train & Test Split 
length(data_close)
training_size=floor(length(data_close)*0.7);training_sizetest_size=length(data_close)-training_size
train_data=data_close[0:74]
head(train_data)
train_data<-ts(train_data,frequency =12,start=2014)
plot(train_data)
test_data=data_close[75:107]
test_data=ts(test_data,frequency = 12,end=2023)
test_data
test_data<-test_data[-33]
length(test_data)
test_data=ts(test_data,frequency = 12,end=2023)
test_data
plot(test_data)
mdl_tr<-arima(x=train_data,order=c(0,1,0),seasonal=list(order=c(0,1,1),frequency=12))
summary(mdl_tr)
plot(mdl_tr)
p1=predict(mdl_tr,n.ahead=32)
Sariam_mse<-sum()
tail(p1$pred)
tail(test_data)
head(p1$pred)
head(test_data)
length(p1)
t1<-p1$pred
plot(p1$pred)
length(t1)
length(test_data)
xlim=range(time(train_data),time(p1$pred))
ylim=range(train_data,p1$pred-2*p1$se,p1$pred+2*p1$se)
plot(train_data,xlim=xlim,ylim=ylim,main="SARIMA Forecasting",xlab="time",ylab="Close-Price",col="red")
lines(p1$pred,lwd=2,col=4)
lines(p1$pred-2*p1$se,lty="dotted",col="darkred")
lines(p1$pred+2*p1$se,lty="dotted",col="darkred")
lines(x=time(test_data), y=test_data, type = 'l', col=3,lwd=2)
Sarima_mse<-sqrt(sum((test_data-t1)^2)/(length(test_data)))
Sarima_mse
legend(2015,600,c("Observed_train","test-obs","test-pred","Std-error band","RMSE=91.001"), lwd=c(5,6,7),
       col=c("red","green","blue","darkred","darkorchid"), y.intersp=1.5)
k<-data.frame(test_data,t1)

