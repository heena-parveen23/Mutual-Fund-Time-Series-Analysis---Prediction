#Project M.sc (Mutual Fund)
data<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/ABFRL.NS (monthly).csv")
head(data)
tail(data)
data_close<-ts(data$Close,start=2014,frequency=12)
b=data_close
str(data)
sum(is.na(data_close))
plot(data_close)
dcom<-decompose(data_close)
plot(dcom)
library(forecast)
library(tseries)
library(zoo)
adf.test(data_close)
# since p-value=0.30>0.05, so we are fail to reject H_o, i.e the series is non-stationary
y=diff(data_close)
auto.arima(y)
str(y)
adf.test(y)
# Here p-value<0.05, the series y is stationary
plot(y)
acf(y,lag.max=50,main="ACF")
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
test_data=ts(test_data,frequency = 12,end=2022)
plot(test_data)
mdl_tr<-arima(x=train_data,order=c(1,1,0),seasonal=list(order=c(0,1,1),frequency=12))
p1=predict(mdl_tr,n.ahead=33)
length(p1)
t1<-p1$pred
length(t1)
length(test_data)
fig1<-plot(train_data,ylab="Pred vs obs",col='blue')
fig2<-plot(test_data,type="l",col="red")
fig3<-plot(t1,type="l",col="green")
library(ggplot2)
length(data_close)
data1<-data.frame(data_close,q$pred,time(data_close))
ggplot()+
  geom_line(data1,mapping=aes(x=time(data_close),y=data_close),color="red")+
  geom_point(data1,mapping=aes(x=time(data_close),y=data_close),color="red")+
  geom_line(data1,mapping=aes(x=time(data_close),y=q$pred),color="blue")+
  geom_point(data1,mapping=aes(x=time(data_close),y=q$pred),color="blue")





##SARIMA fitting
mdl<-arima(x=data_close,order=c(1,1,0),seasonal=list(order=c(0,1,1),frequency=12))
summary(mdl)
qqnorm(mdl$residuals)
## Forecast for next 2 years 
p=predict(mdl,n.ahead=24)
# Plot original & Predicted on same plot
xlim=range(time(data_close),time(p$pred))
ylim=range(data_close,p$pred-2*p$se,p$pred+2*p$se)
plot(data_close,xlim=xlim,ylim=ylim,main="SARIMA Forecasting")
lines(p$pred,lwd=2,col=4)
lines(p$pred-2*p$se,lty="dotted")
lines(p$pred+2*p$se,lty="dotted")

### Experiment
q=predict(mdl)
q
xlim=range(time(data),time(q$pred))
ylim=range(data_close,q$pred)
plot(data_close,xlim=xlim,ylim=ylim)
lines(q$pred,lwd=2,col=4)
### Exponential Technique ###
## Holt Winters Method 
library(forecast)
library(TSA)
data_close<-ts(data$Close,start=2014,frequency=12)
tail(data_close)
plot(data_close)
dcom<-decompose(data_close)
plot(dcom)
# Finding Trend is weather additive or multiplicative
plot(dcom$trend) # trend is multiplicative
plot(dcom$seasonal) # seasonality is additive
fit=HoltWinters(data_close,seasonal="additive")
plot(forecast(fit))
head(data_close)
fitted(fit)
fit$coefficients
fit$alpha
fit$beta
fit$gamma
length(data_close)
RMSE<-sqrt(fit$SSE/length(data_close)); RMSE



plot(fit)
#plot(data_close,col="blue",main="Holt-Winters Smoothing")
#lines(fit$fitted,col='red')
forecast(fit,24) # Forecast for next 2 years
plot(forecast(fit,24))# Plotting of future value
## Measuring Forecast Accuracy
#Residual Plot
plot(residuals(fit))
# Thus, we can see that the above residual plot does not show any pattern in it.
#Hence, we can conclude that the forecasted value are correct.
#fit$SSE
#z<-forecast:::forecast.HoltWinters(model,h=2)
#plot(z)
library(forecast)

## Conclusion:-
#In Tripple exponential smoothing RMSE 26.21 whereas SARIMA fitting RMSE 23.38.
# So in this case SARIMA will fit better for the data.


## Artificial Neural Network Technique for Forecasting Time series
#Install the keras R package
install.packages("keras")
library(keras)
install_keras()
head(b)
str(b)
y_t<-data_close[4:108]
y_t1<-data_close[3:107] # t1=(t-1)
y_t2<-data_close[2:106] # t2=(t-2)
y_t3<-data_close[1:105] #t3=(t-3)
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
ann_data<-cbind(y_t1,y_t2,y_t3,y_t)
head(ann_data)
## Creating ANN Model Using Neuralnet package #####
## Using y_t1, y_t2, y_t3 variable as input or independent variable & y_t as an dependent variable or Output variable###

model1<-neuralnet(y_t~y_t1+y_t2+y_t3,data=ann_data,hidden=0,threshold=0.05)
# Lets Plot & See the layers

plot(model1)

## check the data - actual and Predicted 
final_output=cbind(y_t1,y_t2,y_t3,y_t,as.data.frame(model1$net.result))
colnames(final_output)=c("Input1","Input2","Input3","Expected Output", "Neural Net Output")
print(final_output)
t1<-model1$result.matrix
Prediction<-model1$net.result
Actual<-y_t
head(Actual)
str(y_t)
head(Prediction)
str(Prediction)
# calculating errors i.e MSE

SSE<-26946.69055 # From the Graph Of ANN Model1
MSE<-(SSE/length(y_t))
MSE
RMSE<-sqrt(MSE) ## RMSE of ANN Model1
RMSE

## Further Prediction(n step ahead forecast) Through ANN Model



## In Tripple exponential smoothing RMSE 26.21 whereas SARIMA fitting RMSE 23.38 & RMSE for ANN Model is 16.01984
#So in this case ANN will fit better for the data.


### LSTM (Long Term Short Memory)

### SVM (Support Vector Machine)
library(e1071)
data<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/ABFRL.NS (monthly).csv")
head(data)
tail(data)
data_close<-ts(data$Close,start=2014,frequency=12)
y_t<-data_close[4:108]
y_t1<-data_close[3:107] # t1=(t-1)
y_t2<-data_close[2:106] # t2=(t-2)
y_t3<-data_close[1:105] #t3=(t-3)

## Splitting or adjusting of dataset

x<-data.frame(y_t1,y_t2,y_t3)
str(x)
head(x)
y<-y_t
length(x$y_t1)
length(y)
z=data.frame(x,y)
str(z)
x=as.factor(x)
y=as.factor(y)
y_t1=as.factor(y_t1)
y_t2=as.factor(y_t2)
y_t3=as.factor(y_t3)
str(y)
str(x)
length(x)
str(z)
dim(z)
model_svm<-svm(y_t~.,data=z,method="svmLinear")
summary(model_svm)
print(model_svm)
## Prediction
pred<-predict(model_svm,x)
# Creating Confusion Matrix to evaluate the result of SVM Prediction & the Class data
library(caret)
dm<-caret::confusionMatrix(pred,y)
print(dm)

## Another Process of Creating Confusion Matrix
library(ConfusionTableR)
mc_df<-ConfusionTableR::multi_class_cm(pred,y,mode="everything")
mc_df$confusion_matrix