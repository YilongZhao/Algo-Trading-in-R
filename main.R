
# followings import data, the data has been inversed in csv

BootstrapPrice <- read.csv("E:/NYU/course/3rdSM/algrithemic/Final Proj/BootstrapPrice.csv", stringsAsFactors=FALSE, header =TRUE )

newprice <- as.matrix(BootstrapPrice)
N <- length(BootstrapPrice)


library(TTR)
library(tseries)

# backup test

outcome <- list()
i <- 1
for (i in 1:101) {
  outcome$number[i] <- i-1 # number
  j <- 1
  EMA12 <- EMA(newprice[i, ], n = 12, wilder = FALSE, ratio = NULL)
  EMA26 <- EMA(newprice[i, ], n = 26, wilder = FALSE, ratio = NULL)
  MACD <- EMA12-EMA26
  Signal <-EMA(MACD, n = 9, wilder = FALSE, ratio = NULL)
  
  sellpoint <- 0
  buypoint <- 0 
  action <- 0
  k <- 1
  for (k in 1:N){ # action vector
    if (is.na(newprice[i,k]) || is.na(Signal[k])) action[k]<- "N/A"
    else if (MACD[k] < Signal[k]) action[k]<- "buy"
    else if (MACD[k] > Signal[k]) action[k]<- "sell"
    k <- k+1
  }
  k <- 2
  for (k in 2:N) { # buypoint/sellpoint vector
    if (action[k]=="buy" && action[k-1]=="sell") buypoint <- append(buypoint,k)
    else if (action[k]=="sell" && action[k-1]=="buy") sellpoint <- append(sellpoint,k)
    k <- k+1
  }
  
  if (buypoint[2] < sellpoint[2]) {
    roundtrip <- min(length(buypoint),length(sellpoint))
    buypoint <- buypoint[2:roundtrip]
    sellpoint <- sellpoint[2:roundtrip]
  } else {
    sellpoint <- sellpoint[-2]
    roundtrip <- min(length(buypoint),length(sellpoint))
    buypoint <- buypoint[2:roundtrip]
    sellpoint <- sellpoint[2:roundtrip]
  }
  roundtrip <- roundtrip-1
  outcome$round.trip[i] <- roundtrip # round.trip
  
  # return\trades
  # assume for each round-trip trade, use the same amount principal to buy at buy signal, and sell it all at sell signal.
  
  k <- 1
  return <- 0 # rate of return of each round
  totalreturn <- 1 # overall cumulative rate of return
  drawdown <- 0
  trade <- 0 # absolute value of return
  wintrade <- 0
  mdd <- 0
  for (k in 1:roundtrip){
    return[k] <- newprice[i,sellpoint[k]]/newprice[i,buypoint[k]]
    trade[k] <- newprice[i,sellpoint[k]]-newprice[i,buypoint[k]]
    totalreturn <- totalreturn*return[k]
    if (is.na(trade[k])==FALSE && trade[k] > 0) wintrade <- wintrade+1
    k <- k+1
  }
  outcome$total.return[i] <- totalreturn # total return
  outcome$EAR[i] <- totalreturn^(1/4) # effective annual return
  outcome$win.trade[i] <- wintrade # win trade
  winrate <- wintrade/roundtrip
  outcome$win.rate[i] <- winrate # win rate
  mdd <- maxdrawdown(newprice[i, ])
  outcome$max.drawdown[i] <- mdd$maxdrawdown #max drawdown
  outcome$drawdown.rate[i] <- 1 - newprice[i,mdd$to]/newprice[i,mdd$from] #drawdown rate
  i <- i+1
}


write.table(outcome, file = "outcome.csv", sep = ",", row.names = FALSE)

overall <- list(number <-"average", round.trip <- mean(outcome$round.trip[2:101]), total.return <- mean(outcome$total.return[2:101]), EAR <- mean(outcome$EAR[2:101]), win.trade <- mean(outcome$win.trade[2:101]), win.rate <- mean(outcome$win.rate[2:101]), max.drawdown <- mean(outcome$max.drawdown[2:101]), drawdown.rate <- mean(outcome$drawdown.rate[2:101]))

write.table(overall, file ="outcome.csv", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE) 
  
hist(outcome$total.return, breaks = seq(0,4,0.2) ,main = "total return distribution of 100 bootstraps", xlab = "total return" ,ylim = c(0,30))

t.test(outcome$total.return[1:101], alternative = "two.sided", mu = 1)

