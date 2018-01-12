raw <- read.csv("E:/NYU/course/algo trading/raw.csv", stringsAsFactors=FALSE)
head(raw$Adj.Close)

n <- length(raw$Adj.Close)

library(TTR)
library(tseries)


EMA12 <- EMA(raw$Adj.Close, n = 12, wilder = FALSE, ratio = NULL)
EMA26 <- EMA(raw$Adj.Close, n = 26, wilder = FALSE, ratio = NULL)
MACD <- EMA12-EMA26
Signal <-EMA(MACD, n = 9, wilder = FALSE, ratio = NULL)

head(Signal, n=100)

sellpoint <- 0
buypoint <- 0 ##buypoint and sellpoint contain the data on which actions should be made 
action <- 0
i <- 1
for (i in 1:n){ ##codes for action vector
  if (is.na(raw$Adj.Close[i]) || is.na(Signal[i])) action[i]<- "N/A"
  else if (MACD[i] < Signal[i]) action[i]<- "buy"
  else action[i]<- "sell"
  i <- i+1
}
i <- 2
for (i in 2:n) { ##codes for buypoint/sellpoint vector
  if (action[i]=="buy" && action[i-1]=="sell") buypoint <- append(buypoint,i)
  else if (action[i]=="sell" && action[i-1]=="buy") sellpoint <- append(sellpoint,i)
  i <- i+1
}

# following measures performance

# first, to make it round-trip

tail(buypoint)
tail(sellpoint)
if (buypoint[2] < sellpoint[2]) {
  roundtrip <- min(length(buypoint),length(sellpoint))
  buypoint <- c(buypoint[2:roundtrip])
  sellpoint <- c(sellpoint[2:roundtrip])
} else {
  sellpoint <- c(sellpoint[-2])
  roundtrip <- min(length(buypoint),length(sellpoint))
  buypoint <- c(buypoint[2:roundtrip])
  sellpoint <- c(sellpoint[2:roundtrip])
}
roundtrip <- roundtrip-1
# return\trades
# assume for each round-trip trade, use the same amount principal to buy at buy signal, and sell it all at sell signal.

i <- 1
return <- 0 # rate of return of each round
totalreturn <- 1 # overall cumulative rate of return
drawdown <- 0
trade <- 0 # absolute value of return
wintrade <- 0
for (i in 1:roundtrip){
  return[i] <- (raw$Adj.Close[sellpoint[i]]-raw$Adj.Close[buypoint[i]])/raw$Adj.Close[buypoint[i]]
  trade[i] <- raw$Adj.Close[sellpoint[i]]-raw$Adj.Close[buypoint[i]]
  totalreturn <- totalreturn*(1+return[i])
  if (is.na(trade[i])==FALSE && trade[i] > 0) wintrade <- wintrade+1
  i <- i+1
}
winrate <- wintrade/roundtrip

# outcomes:
# totalreturn
# wintrade

# max drawdown:
library(tseries)
mdd <- maxdrawdown(raw$Adj.Close)
ddrate <- 1-mdd$to/mdd$from
# mdd$maxdrawdown