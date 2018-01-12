# ARMA model stimulation
library(quantmod) 
library(fArma)
raw <- read.csv("E:/NYU/course/3rdSM/algrithemic/Final Proj/raw.csv")

armaSearch = function( #to estimate the best parameters
  xx,  
  minOrder=c(0,0),
  maxOrder=c(5,0),
  trace=FALSE )
{
  bestAic = 1e9
  len = NROW( xx ) 
  for( p in minOrder[1]:maxOrder[1] ) for( q in minOrder[2]:maxOrder[2] )
  {
    if( p == 0 && q == 0 )
    {    
      next
    }    
    
    formula = as.formula( paste( sep="", "xx ~ arma(", p, ",", q, ")" ) )
    
    fit = tryCatch( armaFit( formula, data=xx ),
                    error=function( err ) FALSE,
                    warning=function( warn ) FALSE )
    if( !is.logical( fit ) )
    {    
      fitAic = fit@fit$aic
      if( fitAic < bestAic )
      {    
        bestAic = fitAic
        bestFit = fit
        bestModel = c( p, q )
      }    
      
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): AIC = ", fitAic )
        print( ss ) 
      }    
    }    
    else
    {    
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): None" )
        print( ss ) 
      }    
    }    
  }
  
  if( bestAic < 1e9 )
  {
    return( list( aic=bestAic, fit=bestFit, model=bestModel ) )
  }
  
  return( FALSE )
}

i <- 1
n <- length(raw$Adj.Close)
log.return <- vector()
for (i in 1:n){
  log.return[i] <- log(raw$Adj.Close[i]/raw$Adj.Close[i-1])
}
armaSearch(log.return, minOrder = c(0,0), maxOrder = c(5,5))

##$aic
##[1] -6124.976

##$fit

##Title:
##  ARIMA Modelling 

##Call:
##  armaFit(formula = formula, data = xx)

##Model:
##  ARIMA(2,0,0) with method: CSS-ML

##Coefficient(s):
##  ar1         ar2   intercept  
##0.0309305  -0.0554146   0.0003908  

##Description:
##  Thu Oct 13 12:02:12 2016 by user: zhaoy 


##$model
##[1] 2 0






