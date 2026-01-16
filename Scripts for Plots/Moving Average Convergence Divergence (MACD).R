lapply(c("quantmod", "timeSeries", "moexer", "xts"), require, character.only=T)

MACD.rus <- function(x, s=NULL, e=NULL, ts = 14, data=T){
  
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
    
    p <- cbind(p, D) } # Merge
    
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  x <- as.timeSeries(p)
  
  DF <- NULL # Where to contain data frames
  
  for (i in 1:ncol(x)){ y <- x[,i]
    
    MA <- NULL # Where to put all moving averages
    
    for (m in 1:length(ts)){ l <- NULL # Get averages for all day periods
    
      for (n in 1:(nrow(y) - ts[m] + 1)){
        
        l = rbind.data.frame(l, mean(y[n:(n + ts[m] - 1),])) }
      
      l <- cbind.data.frame(rownames(y)[ts[m]:nrow(y)], l)
      
      colnames(l) <- c("Date", sprintf("MA%s", ts[m]))
      
      if (is.null(MA)){ MA <- l } else { MA <- merge(MA, l, by="Date") } }
      
    df <- data.frame(rownames(y), y)
    
    colnames(df) <- c("Date", colnames(y))
    
    MA <- merge(df, MA, by="Date")
    
    dates <- MA[,1] # Assign variable for dates
    
    MA <- MA[,-1] # Reduce column with dates
    
    rownames(MA) <- dates # set up dates as row names
    
    MA$MACD <- MA[,2] - MA[,3]
    
    f <- NULL
    
    for (n in 1:(nrow(MA)-8)){ f = rbind.data.frame(f, mean(MA[n:(n + 8),4])) }
    
    f <- data.frame(rownames(MA)[9:nrow(MA)], f)
    
    colnames(f) <- c("Date", "Signal")
    
    MA <- data.frame(rownames(MA), MA)
    
    colnames(MA)[1] <- "Date"
    
    MA <- merge(MA, f, by="Date")
    
    dates <- MA[,1] # Assign variable for dates
    
    MA <- MA[,-1] # Reduce column with dates
    
    rownames(MA) <- dates # set up dates as row names
    
    MA$Histogram <- MA[,4] - MA[,5]
    
    MA <- as.timeSeries(MA)
    
    if (is.null(DF)){ DF <- list(MA) } else { DF[[i]] <- MA } } # Put into list
  
  names(DF) <- colnames(x) 
  
  #par(mar = rep(4, 4)) # Define borders of the plot
  
  for (n in 1:length(DF)){
    
    plot(DF[[n]][,4], col = "red", las = 1, xlab = "Trading Days",
         main = sprintf("%s %s", colnames(DF[[n]])[1],
                        "Moving Average Convergence Divergence"))
    
    lines(DF[[n]][,5], col = "blue")
    
    lines(DF[[n]][,6], col = "black", type = "h")
    axis(side = 4, las = 2) # Right Y-Axis Values
    
    grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
    
    abline(h = 0)
  }
}
MACD.rus("GMKN", s="2025-01-01", ts = c(12,26))
