raw <- read.csv("E:/NYU/course/3rdSM/algrithemic/Final Proj/raw.csv")
n <- length(raw$Adj.Close)

log.return <- vector()
error <- vector()
j <- 1
for (j in 1:n){
  log.return[j] <- log(raw$Adj.Close[j]/raw$Adj.Close[j-1])
  error[j] <- log.return[j] - 0.0309305*log.return[j-1] + -0.0554146*log.return[j-2] #residuals have 3 NA. 1,2,3,
}

# r(t) = 0.0003908 + 0.0309305*r(t-1) + -0.0554146*r(t-2) + e(t)

BTSTRreturn <- matrix(nrow = 101, ncol = n, byrow = TRUE) # bootstrap return
BTSTRprice <- matrix(nrow = 101, ncol = n, byrow = TRUE) # bootstrap price

for (i in 1:101) {
  j <- 1
  k <- 1
  bootstrap <- sample(error[c(4:n)], n, replace = TRUE) # bootstrap the residuals
  if (i == 1) {
    for (j in 1:n) { # the first row stores the raw returns
      BTSTRreturn[i,j] <- log.return[j] # log(raw$Adj.Close[j]/raw$Adj.Close[j-1])
      j <- j+1
    }
  } else {
    for (j in 1:3) { # r[1] == P[1]/P[0] == N/A, so bootstrap starts from r[2]
      BTSTRreturn[i,j] <- log.return[j] # log(raw$Adj.Close[j]/raw$Adj.Close[j-1])
      j <- j+1
    }
    for (j in 4:n) {
      BTSTRreturn[i,j] <- 0.0003908 + 0.0309305*BTSTRreturn[i,j-1] + -0.0554146*BTSTRreturn[i,j-2] + bootstrap[j]
      # bootstrapping residuals, use r[i,j-1]...r[i,j-5] to bootstrap the r[i, ]
      j <- j+1
    }
  }
  i <- i+1
}

# put the bootstrapped-return back to get the bootstrapped-prices.

i <- 1
for (i in 1:101) {
  j <- 1
  if ( i == 1) { # the first row stores the raw prices
    for (j in 1:n) {BTSTRprice[i,j] <- raw$Adj.Close[j] 
    j <- j+1
    }
  } else{ 
  for ( j in 1:n) {
    if ( j == 1 ) BTSTRprice[i,j] <- BTSTRprice[i-1,j]
    else BTSTRprice[i,j] <- exp(BTSTRreturn[i,j]) * BTSTRprice[i,j-1]
    j <- j+1
    }
  }
  i <- i+1
}

write.table(BTSTRprice, file = "BootstrapPrice.csv", sep = ",")
# write.table(BTSTRreturn, file = "BootstrapReturn.csv", sep = ",")
