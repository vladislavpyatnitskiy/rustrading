lapply(c("quantmod", "timeSeries", "moexer"), require, character.only=T) # libs

rus.ma.plt <- function(x, s = NULL, e = NULL, data = T, split=T){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
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
    
    D <- xts(D[,1], order.by = as.Date(D[,2])) # Move dates to row names
    
    colnames(D) <- A # Put the tickers in data set
    
    D <- as.timeSeries(D) # Make it time series
    
    if (A == "BELU" & isTRUE(split) &
        (isTRUE(s < "2024-08-15" | e < "2024-08-15") |
         isTRUE(is.null(s) | is.null(e)))){
      
      f <- which(rownames(D) == "2024-08-15")
      
      D[c(1:f),] <- D[c(1:f),] / 8 } # Adjustments for Novabev stock
    
    D <- D[apply(D, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    D <- as.timeSeries(D)
    
    if (is.null(p)){ p <- list(D) } else { p[[A]] <- D } }
    
  for (n in 1:length(p)){ # Plot
    
    chartSeries(
      round(p[[n]], 2), 
      theme = "white",
      name = sprintf("%s Stock Performance", colnames(p[[n]])),
      TA = "addEMA(50, col='purple');addEMA(200, col='red')"
    ) 
  }
}
rus.ma.plt(x = c("LKOH", "BELU"), s = "2022-01-01", data = T)
