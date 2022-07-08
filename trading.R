
require(quantstrat)
require(devtools)
library(zoo)
library(TTR)
library(default)
library(IKTrading)
library(DSTrading)
library(quantmod)
library(PerformanceAnalytics)


#install_github("braverock/blotter") # dependency
x <- new.env()


NameTckr = "SPY"
Data_acq <- quantmod::getSymbols(NameTckr,
                                   src = "yahoo",
                                   index.class = 'Date',
                                   from = "2015/01/01",
                                   env= new.env(),
                                   auto.assign = FALSE)
  
  
OHLC <- Data_acq[,0:4] # open high low close
CloseP <- Data_acq[,4] # close price

# simple moving average
sma <-SMA(Cl(CloseP),n=20)
tail(sma, n = 5)

#exponential moving average
ema <-EMA(Cl(AAPL),n=20)
tail(ema,n=5)

#bollinger bands
bb <-BBands(Cl(AAPL),s.d=2)
tail(bb,n=5)

# Momentum
M <- momentum(Cl(AAPL), n=2)
head (M,n=5)

# ROC
ROC <- ROC(Cl(AAPL),n=2)
# 2-day ROC
head(ROC,n=5)

# MACD
macd <- MACD(Cl(AAPL), nFast=12, nSlow=26,
             nSig=9, maType=SMA)
tail(macd,n=5)

# RSI
rsi = RSI(Cl(AAPL), n=14)
tail(rsi,n=5)

### CHARTs
# SMA
chartSeries(AAPL, theme=chartTheme('white'))

addSMA(n=30,on=1,col = "blue")
addSMA(n=200,on=1,col = "red")

# EMA
chartSeries(AAPL, theme=chartTheme('white'))

addEMA(n=30,on=1,col = "blue")
addEMA(n=200,on=1,col = "red")

# BOLLINGER BANDS
chartSeries(AAPL)
addBBands(n=20,sd=2)

# MOMENTUM
chartSeries(AAPL,
            theme=chartTheme('white'))

addMomentum(n=1)

# ROC
chartSeries(AAPL,
            theme=chartTheme('white'))

addROC(n=7)

# MACD
chartSeries(AAPL,
            theme=chartTheme('white'))

addMACD(fast=12,slow=26,signal=9,type="EMA")

# RSI
chartSeries(AAPL,
            theme=chartTheme('white'))

addRSI(n=14,maType="EMA")

# CUSTOM TA
sma <- SMA(Cl(AAPL),n=14)
chartSeries(AAPL,
            theme=chartTheme('white'))

addTA(sma, on=1, col="red")

# CHART OF PRICE CHANGES OF TWO STOCKS
NameTckr = "AAPL"
Data_acq <- quantmod::getSymbols(NameTckr,
                                 src = "yahoo",
                                 index.class = 'Date',
                                 from = "2015/01/01",
                                 env= new.env(),
                                 auto.assign = FALSE)


OHLC <- Data_acq[,0:4] # open high low close
AAPL <- Data_acq[,4] # close price

NameTckr = "GOOG"
Data_acq <- quantmod::getSymbols(NameTckr,
                                 src = "yahoo",
                                 index.class = 'Date',
                                 from = "2015/01/01",
                                 env= new.env(),
                                 auto.assign = FALSE)


OHLC <- Data_acq[,0:4] # open high low close
GOOG <- Data_acq[,4] # close price

NameTckr = "MSFT"
Data_acq <- quantmod::getSymbols(NameTckr,
                                 src = "yahoo",
                                 index.class = 'Date',
                                 from = "2015/01/01",
                                 env= new.env(),
                                 auto.assign = FALSE)


OHLC <- Data_acq[,0:4] # open high low close
MSFT <- Data_acq[,4] # close price

NS <- function(xdat) xdat / coredata(xdat)[1]
a <- NS(Cl(AAPL))-1
g <- NS(Cl(GOOG))-1
m <- NS(Cl(MSFT))-1
#chartSeries(a,
#            subset = '2007',
#            theme=chartTheme('white'))
#addTA(g, on=1, col="red", lty="dotted")
#addTA(m, on=1, col="blue", lty="dashed") 

chartSeries(MSFT,
            type = 'line',
            theme=chartTheme('white'))


chartSeries(MSFT,
            theme=chartTheme('white'))

chartSeries(MSFT)

addTA(signal,type='S',col='red')

trade <- Lag(signal,1) # trade based on yesterday signal
ret<-dailyReturn(MSFT)*trade
names(ret)<-"filter"
charts.PerformanceSummary(ret, main="Naive Buy Rule")
getSymbols("MSFT")
price <- Cl(MSFT)
r <- price/Lag(price) - 1
delta<-0.005
signal <-c(NA) # first signal is NA

for (i in 2: length(Cl(MSFT))){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(MSFT))

trade1 <- Lag(signal)
ret1<-dailyReturn(MSFT)*trade1
names(ret1) <- 'Naive'
charts.PerformanceSummary(ret1)

## 
