lapply(c("quantmod", "timeSeries", "moexer"), require, character.only=T) # libs

rus.ma.plt <- function(x, s = NULL, e = NULL, data = T){
  
  if (isTRUE(data)){ p <- NULL # Data download loop for 4 scenarios:
      
    for (A in x){ D <- as.data.frame(get_candles(A, s, e,
                                                 interval = 'daily')[,c(3,8)])
    D <- D[!duplicated(D),] # Remove duplicates
    
    p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Remove NA
    
    colnames(p) <- x
    
    x <- p } # Give tickers to data
    
  r <- as.timeSeries(x) # Make data time series
  
  for (n in 1:ncol(r)){
    
    chartSeries(round(r[,n], 2), theme = "white",
                name = sprintf("%s Stock Performance", colnames(r[,n])),
                TA = "addEMA(50, col='purple');addEMA(200, col='red')") }
}
rus.ma.plt(x = "LKOH", s = "2022-01-01", data = T)
