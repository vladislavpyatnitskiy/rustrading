lapply(c("timeSeries", "xts", "moexer"), require, character.only = T) # lib

lines.plt.ma.rus <- function(x, s=NULL, e=NULL, ts=50, data=T, split=T, all=F){
  
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
    
    if (!all) p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
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
    
    MA <- as.timeSeries(MA) # Make it time series
    
    if (is.null(DF)){ DF <- list(MA) } else { DF[[i]] <- MA } } # Put into list
    
  names(DF) <- colnames(x) 
  
  for (n in 1:length(DF)){ p <- DF[[n]]
    
    plot(
      p[,1],
      ylim = c(min(p), max(p)),
      lty = 1,
      type = "l",
      lwd = 2,
      las = 1,
      xlab = "Trading Days",
      ylab = NULL,
      main = sprintf("%s Stock Price with Moving Averages", colnames(p)[1])
    )
    
    axis(side = 4, las = 2) # Right Y-Axis Values
    
    grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
    
    abline(h = 0) # Add black horizontal line at break even point
    
    for (m in 2:(ncol(p))){ lines(p[,m], col = m, lwd = 2) } # Plot indices
    
    par(mar = c(8, rep(4, 3))) # Define borders of the plot
    
    legend(
      x = "bottom",
      inset = c(0, -0.2),
      legend = colnames(p),
      col = seq(ncol(p)),
      lwd = 2,
      cex = .85,
      bty = "n",
      xpd = T,
      horiz = T
    )
    
    on.exit(par(par(no.readonly = T))) } # Show legend with names
}
lines.plt.ma.rus("BELU", s = "2022-01-01", ts=c(50, 200))
