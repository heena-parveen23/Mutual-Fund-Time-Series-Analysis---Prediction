# SARIMA for Reliance
# 
anmutual<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/RELIANCE.NS.csv")
data_close_2<-ts(anmutual$Close,start=2014,frequency=12) # Reliance mutual fund
plot(data_close_2,col="red",xlab="time",ylab="Close-Price",main="Reliance MF")
auto.arima(data_close_2,stepwise = FALSE,approximation = FALSE)
library(forecast)
library(tseries)
library(zoo)
adf.test(data_close_2)
adf.test(diff(data_close_2))
y=diff(data_close_2)
acf(y,lag.max=50,main="ACF of 1st order Diff Series For Reliance MF",col="red")
pacf(y,lag.max=50,main="Pacf of 1st order Diff Series For Reliance MF",col="blue")
t<-diff(y,12)
acf(t,lag.max=100,main="ACF of Seasonal  Diff Series For Reliance MF",col="red")
pacf(t,lag.max=100,main="Pacf of Seasonal Diff Series For Reliance MF",col="blue")
###Train & Test Split 
length(data_close_2)
training_size=floor(length(data_close_2)*0.7);training_sizetest_size=length(data_close_2)-training_size
train_data=data_close_2[0:74]
head(train_data)
train_data<-ts(train_data,frequency =12,start=2014)
plot(train_data)
test_data=data_close_2[75:107]
test_data=ts(test_data,frequency = 12,end=2023)
test_data
test_data<-test_data[-33]
length(test_data)
test_data=ts(test_data,frequency = 12,end=2023)
test_data
plot(test_data)
mdl_tr_2<-arima(x=train_data,order=c(3,1,2),seasonal=list(order=c(1,1,0),frequency=12))
summary(mdl_tr_2)
p1=predict(mdl_tr_2,n.ahead=32)
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
length(t1)
xlim=range(time(train_data),time(p1$pred))
ylim=range(train_data,p1$pred-2*p1$se,p1$pred+2*p1$se)
plot(train_data,xlim=xlim,ylim=ylim,main="SARIMA Fitting For Reliance MF",xlab="time",ylab="Close-Price",col="red")
lines(p1$pred,lwd=2,col=4)
lines(p1$pred-2*p1$se,lty="dotted",col="darkred")
lines(p1$pred+2*p1$se,lty="dotted",col="darkred")
lines(x=time(test_data), y=test_data, type = 'l', col=3,lwd=2)

legend(2015,2000,c("Observed_train","test-obs","test-pred","Std-error band","RMSE=250.001"), lwd=c(5,6,7),
       col=c("red","green","blue","darkred","darkorchid"), y.intersp=1.5)
sqrt(620.1306)
Sarima_Rmse<-sqrt(sum((test_data-t1)^2)/(length(test_data)))
Sarima_mse
## Exponential Smoothing
anmutual<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/RELIANCE.NS.csv")
data_close_2<-ts(anmutual$Close,start=2014,frequency=12) # Reliance mutua
plot(data_close)
train_data_close_2<-data_close_2[0:74]
test_data_close_2<-data_close_2[75:107]
length(data_close)
tail(data_close)
train_data_exp<-ts(train_data_close_2,start = 2014,frequency = 12)
test_data_exp<-ts(test_data_close_2,frequency = 12,end=2023)
head(test_data_exp)
tail(test_data_exp)
plot(train_data_exp)
plot(test_data_exp)
dcom<-decompose(data_close_2)
dcom_train_exp<-decompose(train_data_exp)
plot(dcom_train_exp)
# Finding Trend is weather additive or multiplicative
plot(dcom$trend) # trend is multiplicative
plot(dcom$seasonal) # seasonality is additive
fit2=HoltWinters(train_data_exp,seasonal="multiplicative")
fit2$coefficients
fit2$alpha
fit2$beta
fit2$gamma
length(data_close_2)
RMSE<-sqrt(fit2$SSE/length(train_data_exp)); RMSE
#plot(data_close,col="blue",main="Holt-Winters Smoothing")
#lines(fit$fitted,col='red')
z<-forecast(fit2,34);z # Forecast for next 2 years
plot(forecast(fit2,34))# Plotting of future value
lines(x=time(test_data_exp), y=test_data_exp, type = 'l', col=3,lwd=2)
legend(2016,2500,c("test-pred","test-obs","RMSE=300"), lwd=c(1,2,2), col=c("blue","green","red"))
   
# ANN for Reliance
anmutual<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/RELIANCE.NS.csv")
data_close_2<-ts(anmutual$Close,start=2014,frequency=12) # Reliance mutua
y_t<-data_close_2[4:108]
y_t1<-data_close_2[3:107] # t1=(t-1)
y_t2<-data_close_2[2:106] # t2=(t-2)
y_t3<-data_close_2[1:105] #t3=(t-3)
length(y_t)
length(y_t1)
length(y_t2)
length(y_t3)
plot(y_t1,y_t,main="scatter Plot of y_(t-1) & y_t",col="red")
plot(y_t2,y_t,main="scatter Plot of y_(t-2) & y_t",col="blue")
plot(y_t3,y_t,main="scatter Plot of y_(t-3) & y_t",col="green")
cor(y_t1,y_t)
cor(y_t2,y_t)
cor(y_t3,y_t)
ann_data<-data.frame(y_t1,y_t2,y_t3,y_t)
length(ann_data$y_t)

head(ann_data)
sapply(ann_data,class)
## Spliting training set & testing set

trainset <-ann_data[1:73, ]

testset <-ann_data[74:105, ] # Containing all the variables 
plot(testset$y_t)
t
head(testset$y_t)
time(testset$y_t)

time(ann_data$y_t[74]:ann_data$y_t[105])
testdata<-data.frame(y_t1,y_t2,y_t3)[74:105,] # except dependent variable
head(testdata)
time(testdata$y_t1)
head(testset)
head(testset$y_t)
## Creating ANN Model Using Neuralnet package #####
## Using y_t1, y_t2, y_t3 variable as input or independent variable & y_t as an dependent variable or Output variable###
library(neuralnet)
# Modeling using training set
model2<-neuralnet(y_t~y_t1+y_t2+y_t3,data=trainset,hidden=0,err.fct="sse",threshold=0.05,linear.output=TRUE)
model2$result.matrix
plot(model2)

# Lets Plot & See the layers



## Prediction for test data set

pred=compute(model2,testdata)
pred1=compute(model2,trainset)
train_pred=pred1$net.result






## Model Validation ###
test <- data.frame(actual=testset$y_t,prediction =pred$net.result )
## Original train value

ann_data
plot(ann_data$y_t, type = 'l',xlab="time",ylab="Close- Price",main ="ANN Original & Prediction For Reliance MF",lwd=3,col=1)
train_pred
lines(x=seq(1,73), y=train_pred, type = 'l', col = 4,lwd=3)
test$prediction
lines(x=seq(74,105), y=test$prediction, type = 'l', col=6,lwd=3)
legend(20,2500,c("Observed","train-pred","test-pred","RMSE=55.42"), lwd=c(5,6,7,3), col=c(1,4,6,3), y.intersp=1.5)

RMSE <-sqrt(224225/73)# For ANN Reliance Data
RMSE
       