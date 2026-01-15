lapply(c("quantmod", "timeSeries", "moexer"), require, character.only=T) # libs

rus.ma.plt <- function(x, s = NULL, e = NULL, data = T){
  
  if (data){ p <- NULL # Data download loop for 4 scenarios:
    
    getData <- function(A, s, e) { 
      if (is.null(s) && is.null(e))
        return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
      if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
      if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
      return(get_candles(A, from = s, till = e, interval = 'daily')) 
    }
    for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])
    
      message(
        sprintf(
          "%s is downloaded (%s / %s)", 
          A, which(x == A), length(x)
        )
      ) # Download message
      
      D <- D[!duplicated(D),] # Remove duplicates
      
      p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
      
      p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Remove NA
      
      colnames(p) <- x
      
      x <- p } # Give tickers to data
      
    r <- as.timeSeries(x) # Make data time series
    
    for (n in 1:ncol(r)){ # Plot
      
      chartSeries(
        round(r[,n], 2), 
        theme = "white",
        name = sprintf("%s Stock Performance", colnames(r[,n])),
        TA = "addEMA(50, col='purple');addEMA(200, col='red')"
      ) 
      }
}
rus.ma.plt(x = "LKOH", s = "2022-01-01", data = T)
