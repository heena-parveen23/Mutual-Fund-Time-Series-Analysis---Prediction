anmutual<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/RELIANCE.NS.csv")
data_close<-anmutual$Close
data_close_2<-ts(anmutual$Close,start=2014,frequency=12) # Reliance mutual fund

head(data_close_2)
data<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/ABFRL.NS (monthly).csv")
head(data)
tail(data)
data_close_1<-ts(data$Close,start=2014,frequency=12) # ABFRL
head(data_close_1)
#Cross Spectral Analysis


x<-ts.union(data_close_1,data_close_2)

ccf(data_close_1,data_close_2,main="Cross-Correlation")# Cross Correlation Function

# Stock Return
library(tseries)
library(forecast)
adj1<-ts(data$Adj.Close,start=2014,frequency=12)
adj2<-ts(anmutual$Adj.Close,start=2014,frequency = 12)
plot(adj1)
plot(adj2)
mdl1<-arima(x=adj1,order=c(1,1,0),seasonal=list(order=c(1,1,0),frequency=12))
mdl2<-arima(x=adj2,order=c(3,1,2),seasonal=list(order=c(0,1,2),frequency=12))
p1=predict(mdl1,n.ahead=24)# Further 2 year Forecast of Adj Close Price, SARIMA ABFRL
p2=predict(mdl2,n.ahead =24)# Further 2 year Forecast of Adj Close, by SARIMA Reliance
t1<-p1$pred
t1
head(t1)
t2<-p2$pred

# Stock Return = [Previous day adjusted/Today Adjusted]-1
# For 2 years
length(t1)
length(t2)
r1=c()
r2=c()
for ( i in 1 : 24)
{
  r1[i+1]= round(((t1[i]/t1[i+1])-1)*100,2)
}

for (i in 1:24){
  r2[i+1]= round(((t2[i]/t2[i+1])-1)*100,2)
}
r2
r1
# Aditya Birla Return for 2 years is 4.01% whereas Reliance Return is 0.83%.So We need to invest in Aditya Birla Mutual Fund
# For 5 years 
p3=predict(mdl1,n.ahead=60)# Further 5 year Forecast of Adj Close Price, SARIMA ABFRL
p4=predict(mdl2,n.ahead =60)# Further 5 year Forecast of Adj Close, by SARIMA Reliance
t3<-p3$pred
t3
t4<-p4$pred;t4


# Stock Return = [Previous day adjusted/Today Adjusted]-1
# For 5 years
length(t3)
length(t4)
r3=c()
r4=c()
for ( i in 1 : 60)
{
  r3[i]= round(((t3[i-1]/t3[i])-1)*100,2)
}

for (i in 1:60){
  r4[i]= round(((t4[i-1]/t4[i])-1)*100,2)
}
r3
r4
## Aditya Birla Return for 5 years is 2.01% whereas Reliance Return is 0.67%.So Again We need to invest in Aditya Birla Mutual Fund rather than Reliance.
# Forecast Graph for 5 years ABFRL
xlim=range(time(adj1),time(p3$pred))
ylim=range(adj1,p3$pred-2*p3$se,p3$pred+2*p3$se)
plot(adj1,xlim=xlim,ylim=ylim,main="SARIMA Forecasting For ABFRL",xlab="time",ylab="Adj-Close-Price",col="red")
lines(p3$pred,lwd=2,col="blue")
lines(p3$pred-2*p3$se,lty="dotted",col="darkred")
lines(p3$pred+2*p3$se,lty="dotted",col="darkred")
legend(2016,1200,c("Observed","Forecast","Std.Error"), lwd=c(5,6,7), col=c("red","blue","darkred"), y.intersp=1.5)
# Forecasting Graph of Reliance for 5 years
xlim=range(time(adj2),time(p4$pred))
ylim=range(adj2,p4$pred-2*p4$se,p4$pred+2*p4$se)
plot(adj2,xlim=xlim,ylim=ylim,main="SARIMA Forecasting For Reliance",xlab="time",ylab="Adj-Close-Price",col="red")
lines(p4$pred,lwd=2,col="green")
lines(p4$pred-2*p4$se,lty="dotted",col="darkred")
lines(p4$pred+2*p4$se,lty="dotted",col="darkred")
legend(2016,5000,c("Observed","Forecast","Std.Error"), lwd=c(5,6,7), col=c("red","green","darkred"), y.intersp=1.5)

X<-c(1,2)
Y<-c(2,4)
plot(X,Y)
 # Prediction for ANN 
y<-c(2,4,5,8,9,10)
y1<-y
for(i in (length(y)+1) :(length(y)+5)){
  yhat=2.5+3.6*y1[i-1]+2.7*y1[i-2]
  y1=append(y1,yhat)
}
y1
y1=round(setdiff(y1,y),2)
y1


## Taking Two adj close price series
data<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/ABFRL.NS (monthly).csv")

data_close_adj_1<-ts(data$Adj.Close,start=2014,frequency=12) # ABFRL

anmutual<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/RELIANCE.NS.csv")

data_close_adj_2<-ts(anmutual$Adj.Close,start=2014,frequency=12) # Reliance mutual fund

y_t<-data_close_adj_1[4:108]
y_t1<-data_close_adj_1[3:107] # t1=(t-1)
y_t2<-data_close_adj_1[2:106] # t2=(t-2)
y_t3<-data_close_adj_1[1:105] #t3=(t-3)
data1<-data.frame(y_t1,y_t2,y_t3,y_t)
library(neuralnet)
# Modeling using training set
model1<-neuralnet(y_t~y_t1+y_t2+y_t3,data=data1,hidden=0,err.fct="sse",threshold=0.05,linear.output=TRUE)
model1$result.matrix
plot(model1)
#𝑌_𝑡=(18.16+0.91𝑌_(𝑡−1)+0.015𝑌_(𝑡−2)−0.014𝑌_(𝑡−3))  for Aditya Birla Mutual Fund
#𝑌_𝑡=(19.08+0.85𝑌_(𝑡−1)+0.13𝑌_(𝑡−2)+0.012𝑌_(𝑡−3))   for   Reliance Mutual Fund 

