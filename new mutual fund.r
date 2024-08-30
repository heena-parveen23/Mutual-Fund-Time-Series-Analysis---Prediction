anmutual<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/RELIANCE.NS.csv")
head
data_close<-anmutual$Close
data_close_2<-ts(anmutual$Close,start=2014,frequency=12) # Reliance mutual fund

head(data_close_2)
data<-read.csv("C:/Users/UJJWAL GHARUI/Downloads/ABFRL.NS (monthly).csv")
head(data)
tail(data)
data_close_1<-ts(data$Close,start=2014,frequency=12)
head(data_close_1)
#Cross Spectral Analysis
library(tskernal)
install.packages("tskernal")
library("psd")
x<-ts.union(data_close_1,data_close_2)
z<-spectrum(data_close_1,data_close_2,span=9)
plot(coherence(z),xlim=c(0,0.2))
plot(gain(z))
plot(impulse.response(z))
ccf(data_close_1,data_close_2,main="Cross-Correlation")# Cross Correlation Function










# Stock Return
library(tseries)
library(forecast)
adj1<-ts(data$Adj.Close,start=2014,frequency=12)
adj2<-ts(anmutual$Adj.Close,start=2014,frequency = 12)
plot(adj1)
plot(adj2)
#mdl1<-arima(x=adj1,order=c(0,1,0),seasonal=list(order=c(0,1,1),frequency=12))
#mdl2<-arima(x=adj2,order=c(3,1,2),seasonal=list(order=c(1,1,0),frequency=12))
#summary(md)
#p1=predict(mdl1,n.ahead=24)# Further 2 year Forecast of Adj Close Price, SARIMA ABFRL
#p2=predict(x=mdl2,n.ahead =24)# Further 2 year Forecast of Adj Close, by SARIMA Reliance
#t1<-p1$pred
#t1
#head(t1)
#t2<-p2$pred

### Now we need f

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
# Aditya Birla Return for 2 years is 4.01% whereas Reliance Return is 0.83%.So We need to invest in Aditya Birla Mutual Fundp3<-
